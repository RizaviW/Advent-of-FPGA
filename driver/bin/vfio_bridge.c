/* vfio_bridge.c - A robust bridge between OCaml and Linux VFIO */
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/vfio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

/* * Structure to hold all necessary context.
 * The caller is responsible for freeing this and closing the FDs.
 */
typedef struct {
    int container_fd;
    int group_fd;
    int device_fd;
} vfio_context_t;

/* * Returns 0 on success, negative error code on failure.
 * On success, ctx is populated with valid FDs.
 */
int c_init_vfio(int group_id, const char* pci_addr, vfio_context_t* ctx) {
    int container, group, device;
    struct vfio_group_status group_status = { .argsz = sizeof(group_status) };
    char path[64];

    /* 1. Open Container */
    container = open("/dev/vfio/vfio", O_RDWR);
    if (container < 0) {
        return -1;
    }

    /* 2. Check Extension */
    if (ioctl(container, VFIO_CHECK_EXTENSION, VFIO_TYPE1_IOMMU) != 1) {
        close(container);
        return -2;
    }

    /* 3. Open Group */
    snprintf(path, sizeof(path), "/dev/vfio/%d", group_id);
    group = open(path, O_RDWR);
    if (group < 0) {
        close(container);
        return -3;
    }

    /* 4. Check Group Status */
    if (ioctl(group, VFIO_GROUP_GET_STATUS, &group_status) < 0 ||
       !(group_status.flags & VFIO_GROUP_FLAGS_VIABLE)) {
        close(group);
        close(container);
        return -4;
    }

    /* 5. Add Group to Container */
    if (ioctl(group, VFIO_GROUP_SET_CONTAINER, &container) < 0) {
        close(group);
        close(container);
        return -5;
    }

    /* 6. Set IOMMU Type */
    if (ioctl(container, VFIO_SET_IOMMU, VFIO_TYPE1_IOMMU) < 0) {
        close(group);
        close(container);
        return -6;
    }

    /* 7. Get Device FD */
    device = ioctl(group, VFIO_GROUP_GET_DEVICE_FD, pci_addr);
    if (device < 0) {
        close(group);
        close(container);
        return -7;
    }

    /* Success */
    ctx->container_fd = container;
    ctx->group_fd = group;
    ctx->device_fd = device;

    return 0;
}

int c_get_bar_info(int device_fd, int bar_index, unsigned long* offset, unsigned long* size) {
    struct vfio_region_info reg = { .argsz = sizeof(reg) };
    reg.index = bar_index;

    if (ioctl(device_fd, VFIO_DEVICE_GET_REGION_INFO, &reg) < 0) {
        return -1;
    }

    *offset = reg.offset;
    *size = reg.size;
    return 0;
}
