"""X11 integration check of the example tree; needs libX11, libXext and libXtst.

Run with python3 test_hover.py after building project1.lpi for GTK2, GTK3, Qt5
or Qt6; --dpi and --dpi-matrix require GTK.
This briefly moves the real pointer, restores it, and closes only its own host.
All clicks target the demo's own windows and are only sent after checking that
the own main form is below the pointer; the popup's pointer grab, which makes
clicks on other applications close the menu, is queried instead of exercised.
An optional PNG output path saves the expanded popup for visual inspection.
Use --captions for clipping/line-break checks and --dpi for process-local DPI tests.
Use --full-ring to replace the tree by the chain A -> B -> (C1, C2) and check that
the single root A is already open when the popup appears below the pointer, and
that the full ring formed by B and the root disc stay visible together.
The click fixture makes the second root a leaf only inside the test process
and reports lines appended to the demo memo, which leaf clicks must produce.
It also reports the control hosting the radial menu (the demo's Panel2). The
menu opens at the host point nearest to the form center, so that the popup and
all outside-click targets stay above the own main form.
"""

import ctypes as C
import ctypes.util
import argparse
import os
import re
import math
from pathlib import Path
import select
import subprocess
import struct
import sys
import time
import zlib

parser = argparse.ArgumentParser()
parser.add_argument("snapshot", nargs="?", help="Optional popup PNG path")
parser.add_argument("--dpi", type=int, help="Isolated GTK/LCL test DPI")
parser.add_argument("--captions", action="store_true", help="Test long two-line captions and clipping")
parser.add_argument("--dpi-matrix", action="store_true", help="Compare caption/geometry scaling at 96/144/192 DPI")
parser.add_argument("--full-ring", action="store_true", help="Test a full ring around the root disc")
args = parser.parse_args()
if args.dpi is not None and args.dpi <= 0:
    parser.error("--dpi must be positive")
if args.full_ring and args.captions:
    parser.error("--full-ring and --captions are separate test modes")
if args.dpi_matrix:
    if args.dpi is not None or args.captions or args.snapshot or args.full_ring:
        parser.error("--dpi-matrix is a standalone test mode")
    spans = {}
    for dpi in (96, 144, 192):
        result = subprocess.run(
            [sys.executable, str(Path(__file__).resolve()), "--captions", "--dpi", str(dpi)],
            text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=60,
        )
        print(result.stdout, end="")
        if result.returncode != 0:
            raise RuntimeError(f"Caption/hover test failed at {dpi} DPI (exit {result.returncode})")
        metrics = re.search(r"CAPTION_METRICS dpi=(\d+) width=(\d+) span=(\d+) lines=2", result.stdout)
        if metrics is None:
            raise RuntimeError(f"Missing caption metrics at {dpi} DPI")
        spans[dpi] = int(metrics.group(3))
    for dpi, span in spans.items():
        expected = spans[96] * dpi / 96
        if abs(span - expected) > 3:
            raise AssertionError(f"Caption height at {dpi} DPI: {span}px instead of approximately {expected}px")
    print("PASS: geometry and caption height scale proportionally at 96/144/192 DPI")
    sys.exit(0)


def load(name):
    path = ctypes.util.find_library(name)
    if not path:
        raise RuntimeError(f"Required native library not found: {name}")
    return C.CDLL(path)


x, ext, xt = load("X11"), load("Xext"), load("Xtst")
D, W = C.c_void_p, C.c_ulong
x.XOpenDisplay.argtypes = [C.c_char_p]
x.XOpenDisplay.restype = D
x.XDefaultRootWindow.argtypes = [D]
x.XDefaultRootWindow.restype = W
x.XInternAtom.argtypes = [D, C.c_char_p, C.c_int]
x.XInternAtom.restype = W
x.XGetWindowProperty.argtypes = [
    D, W, W, C.c_long, C.c_long, C.c_int, W, C.POINTER(W),
    C.POINTER(C.c_int), C.POINTER(W), C.POINTER(W),
    C.POINTER(C.POINTER(C.c_ubyte)),
]
x.XQueryTree.argtypes = [
    D, W, C.POINTER(W), C.POINTER(W),
    C.POINTER(C.POINTER(W)), C.POINTER(C.c_uint),
]
x.XFetchName.argtypes = [D, W, C.POINTER(C.c_void_p)]
x.XTranslateCoordinates.argtypes = [
    D, W, W, C.c_int, C.c_int, C.POINTER(C.c_int),
    C.POINTER(C.c_int), C.POINTER(W),
]
x.XQueryPointer.argtypes = [
    D, W, C.POINTER(W), C.POINTER(W), C.POINTER(C.c_int),
    C.POINTER(C.c_int), C.POINTER(C.c_int), C.POINTER(C.c_int),
    C.POINTER(C.c_uint),
]
x.XFree.argtypes = [C.c_void_p]
x.XFlush.argtypes = [D]
x.XCloseDisplay.argtypes = [D]
x.XGetImage.argtypes = [
    D, W, C.c_int, C.c_int, C.c_uint, C.c_uint, W, C.c_int,
]
x.XGetImage.restype = D
x.XGetPixel.argtypes = [D, C.c_int, C.c_int]
x.XGetPixel.restype = W
x.XDestroyImage.argtypes = [D]
x.XGrabPointer.argtypes = [D, W, C.c_int, C.c_uint, C.c_int, C.c_int, W, W, W]
x.XUngrabPointer.argtypes = [D, W]
xt.XTestFakeMotionEvent.argtypes = [D, C.c_int, C.c_int, C.c_int, W]
xt.XTestFakeButtonEvent.argtypes = [D, C.c_uint, C.c_int, W]


class Rect(C.Structure):
    _fields_ = [
        ("x", C.c_short), ("y", C.c_short),
        ("width", C.c_ushort), ("height", C.c_ushort),
    ]


ext.XShapeGetRectangles.argtypes = [
    D, W, C.c_int, C.POINTER(C.c_int), C.POINTER(C.c_int),
]
ext.XShapeGetRectangles.restype = C.POINTER(Rect)


def check(condition, message):
    if not condition:
        raise AssertionError(message)


class XErrorEvent(C.Structure):
    _fields_ = [
        ("type", C.c_int), ("display", D), ("resourceid", W), ("serial", W),
        ("error_code", C.c_ubyte), ("request_code", C.c_ubyte), ("minor_code", C.c_ubyte),
    ]


x_errors = []


@C.CFUNCTYPE(C.c_int, D, C.POINTER(XErrorEvent))
def on_x_error(display, event):
    # Xlib would exit on any error. Closing popups can vanish during window
    # enumeration (BadWindow = 3); the failed call then reports that itself.
    if event.contents.error_code != 3:
        x_errors.append((event.contents.error_code, event.contents.request_code))
    return 0


x.XSetErrorHandler.argtypes = [type(on_x_error)]
x.XSetErrorHandler.restype = C.c_void_p
x.XSetErrorHandler(on_x_error)


d = x.XOpenDisplay(None)
check(d, "Cannot open X11 display")
root = x.XDefaultRootWindow(d)
pid_atom = x.XInternAtom(d, b"_NET_WM_PID", False)
rr, child = W(), W()
rx, ry, wx, wy, state = C.c_int(), C.c_int(), C.c_int(), C.c_int(), C.c_uint()
x.XQueryPointer(d, root, C.byref(rr), C.byref(child), C.byref(rx),
                C.byref(ry), C.byref(wx), C.byref(wy), C.byref(state))
check(not state.value & (256 | 512 | 1024), "A mouse button is held")
saved_pointer = rx.value, ry.value
process = None
output = None
early_output = bytearray()


def stop_demo():
    global output
    if process is None or output is not None:
        return
    process.terminate()
    try:
        rest = process.communicate(timeout=3)[0]
    except subprocess.TimeoutExpired:
        process.kill()
        rest = process.communicate()[0]
    output = (bytes(early_output) + rest).decode(errors="replace")


def wait_output_line(prefix, timeout=10):
    # Reads the pipe unbuffered, so that communicate() in stop_demo continues
    # exactly where this stopped; stop_demo prepends the bytes read here.
    deadline = time.monotonic() + timeout
    while True:
        for line in bytes(early_output).split(b"\n")[:-1]:
            text = line.decode(errors="replace")
            if text.startswith(prefix):
                return text[len(prefix):]
        remaining = deadline - time.monotonic()
        check(remaining > 0, f"Demo did not report {prefix.strip()}")
        if select.select([process.stdout], [], [], remaining)[0]:
            chunk = os.read(process.stdout.fileno(), 4096)
            if not chunk:
                raise RuntimeError("Demo exited early:\n" + early_output.decode(errors="replace"))
            early_output.extend(chunk)


def children(window):
    r, parent, count, ptr = W(), W(), C.c_uint(), C.POINTER(W)()
    if not x.XQueryTree(d, window, C.byref(r), C.byref(parent),
                        C.byref(ptr), C.byref(count)):
        return []  # The window vanished during enumeration.
    result = list(ptr[:count.value])
    x.XFree(ptr)
    return result


def window_pid(window):
    typ, fmt, count, remaining = W(), C.c_int(), W(), W()
    ptr = C.POINTER(C.c_ubyte)()
    if x.XGetWindowProperty(
        d, window, pid_atom, 0, 1, False, 0, C.byref(typ), C.byref(fmt),
        C.byref(count), C.byref(remaining), C.byref(ptr),
    ) != 0:
        return None  # The window vanished during enumeration.
    value = C.cast(ptr, C.POINTER(W))[0] if count.value and fmt.value == 32 else None
    if ptr:
        x.XFree(ptr)
    return value


def own_windows(window=root, depth=3):
    result = []
    for candidate in children(window):
        if window_pid(candidate) == process.pid:
            result.append(candidate)
        elif depth:
            result.extend(own_windows(candidate, depth - 1))
    return result


def name(window):
    ptr = C.c_void_p()
    x.XFetchName(d, window, C.byref(ptr))
    result = C.string_at(ptr).decode(errors="replace") if ptr else ""
    if ptr:
        x.XFree(ptr)
    return result


def find(title):
    return next((w for w in own_windows() if name(w) == title), None)


def wait_window(title):
    for _ in range(60):
        window = find(title)
        if window:
            time.sleep(0.2)
            return window
        if process.poll() is not None:
            raise RuntimeError((bytes(early_output) + process.stdout.read()).decode(errors="replace"))
        time.sleep(0.05)
    raise RuntimeError("Window did not appear: " + title)


def screen_position(window, px, py):
    sx, sy, ch = C.c_int(), C.c_int(), W()
    check(x.XTranslateCoordinates(d, window, root, px, py, C.byref(sx),
                                  C.byref(sy), C.byref(ch)), "Coordinate translation failed")
    return sx.value, sy.value


def move_screen(px, py):
    check(xt.XTestFakeMotionEvent(d, -1, px, py, 0), "Pointer movement failed")
    x.XFlush(d)
    time.sleep(0.15)


def move(window, px, py):
    move_screen(*screen_position(window, px, py))


def click(button):
    check(xt.XTestFakeButtonEvent(d, button, True, 0), "Button press failed")
    check(xt.XTestFakeButtonEvent(d, button, False, 0), "Button release failed")
    x.XFlush(d)
    time.sleep(0.2)


def pointer_chain():
    # Windows containing the pointer, from the root down to the innermost one;
    # grabs do not affect this, windows of other programs on top do.
    chain, window = [], root
    while window:
        chain.append(window)
        r, child = W(), W()
        px, py, cx, cy, mask = C.c_int(), C.c_int(), C.c_int(), C.c_int(), C.c_uint()
        if not x.XQueryPointer(d, window, C.byref(r), C.byref(child), C.byref(px),
                               C.byref(py), C.byref(cx), C.byref(cy), C.byref(mask)):
            break
        window = child.value
    return chain


def pointer_grabbed():
    # An active grab of another client makes our own attempt fail (AlreadyGrabbed = 1).
    status = x.XGrabPointer(d, root, False, 4, 1, 1, 0, 0, 0)
    if status == 0:
        x.XUngrabPointer(d, 0)
        x.XFlush(d)
    return status == 1


def raw_pixel(window, px, py):
    image = x.XGetImage(d, window, px, py, 1, 1, 0xFFFFFFFF, 2)
    check(image, "XGetImage failed")
    try:
        return x.XGetPixel(image, 0, 0)
    finally:
        x.XDestroyImage(image)


def pixel(window, px, py):
    return raw_pixel(window, px, py) & 0xFFFFFF


def shape(window, kind):
    count, ordering = C.c_int(), C.c_int()
    rects = ext.XShapeGetRectangles(d, window, kind, C.byref(count), C.byref(ordering))
    check(rects, "XShapeGetRectangles failed")
    try:
        return tuple((r.x, r.y, r.width, r.height) for r in rects[:count.value])
    finally:
        x.XFree(rects)


def area(rectangles):
    return sum(w * h for _, _, w, h in rectangles)


def wait_for_pixels(predicate, message):
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        if predicate():
            return
        time.sleep(0.02)
    raise AssertionError(message)


def save_snapshot(window, size, has_alpha, path):
    image = x.XGetImage(d, window, 0, 0, size, size, 0xFFFFFFFF, 2)
    check(image, "Cannot capture popup")
    rows = bytearray()
    try:
        for py in range(size):
            rows.append(0)
            for px in range(size):
                value = x.XGetPixel(image, px, py)
                alpha = (value >> 24) & 255 if has_alpha else 255
                for shift in (16, 8, 0):
                    channel = (value >> shift) & 255
                    if 0 < alpha < 255:
                        channel = min(255, (channel * 255 + alpha // 2) // alpha)
                    rows.append(channel if alpha else 0)
                rows.append(alpha)
    finally:
        x.XDestroyImage(image)

    def chunk(tag, data):
        return (struct.pack(">I", len(data)) + tag + data +
                struct.pack(">I", zlib.crc32(tag + data) & 0xFFFFFFFF))

    header = struct.pack(">IIBBBBB", size, size, 8, 6, 0, 0, 0)
    Path(path).write_bytes(b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", header) +
                          chunk(b"IDAT", zlib.compress(rows)) + chunk(b"IEND", b""))


def caption_pixels(window, center, scale, sector):
    radius = round(70 * scale)
    image = x.XGetImage(d, window, center - radius, center - radius,
                        2 * radius + 1, 2 * radius + 1, 0xFFFFFFFF, 2)
    check(image, "Cannot capture caption pixels")
    try:
        for dy in range(-radius, radius + 1):
            for dx in range(-radius, radius + 1):
                r = math.hypot(dx, dy)
                angle = (math.degrees(math.atan2(dy, dx)) + 90) % 360
                offset = angle % 120
                margin = r * math.sin(math.radians(min(offset, 120 - offset)))
                if (8 * scale < r < 64 * scale and
                        int(angle // 120) == sector and margin > 4 * scale):
                    yield dy, x.XGetPixel(image, dx + radius, dy + radius) & 0xFFFFFF
    finally:
        x.XDestroyImage(image)


def check_caption_lines(window, center, scale, selected):
    rows = sorted({dy for dy, color in caption_pixels(window, center, scale, 0)
                   if color != selected})
    groups = []
    for dy in rows:
        if not groups or dy != groups[-1][-1] + 1:
            groups.append([])
        groups[-1].append(dy)
    check(len(groups) == 2, f"Explicit line break produced {len(groups)} visible text bands")
    return rows[-1] - rows[0] + 1


def check_caption_clip(window, center, scale, selected):
    pixels = list(caption_pixels(window, center, scale, 2))
    check(pixels, "No neighboring-sector pixels were checked")
    check(all(color == selected for _, color in pixels),
          "Long caption painted into the neighboring selected sector")


def check_full_ring(popup, kind, point_at):
    # Visible means opaque for RGBA windows and inside the window shape otherwise.
    def visible(point):
        if kind == 2:
            return raw_pixel(popup, *point) >> 24 == 255
        return any(rx <= point[0] < rx + rw and ry <= point[1] < ry + rh
                   for rx, ry, rw, rh in shape(popup, kind))

    # The probes avoid the captions at 90 (A, B), 0 (C1) and 180 (C2) degrees.
    disc = [point_at(20, 270), point_at(50, 330)]
    ring = point_at(95, 270)
    leaves = [point_at(145, 300), point_at(145, 120)]
    # The pointer has not moved since the popup appeared below it.
    wait_for_pixels(lambda: visible(ring) and pixel(popup, *disc[0]) != pixel(popup, *ring),
                    "The single root was not open when the popup appeared")
    check(all(visible(point) for point in disc), "The root disc vanished when the full ring opened")
    selected, neutral = pixel(popup, *disc[0]), pixel(popup, *ring)
    move(popup, *point_at(40, 0))
    check(pixel(popup, *disc[0]) == selected and pixel(popup, *ring) == neutral,
          "Hovering the open single root changed its state")
    move(popup, *point_at(95, 0))
    wait_for_pixels(lambda: pixel(popup, *ring) == selected and
                    all(visible(point) for point in leaves),
                    "Hovering the full ring did not open its children")
    check(all(visible(point) and pixel(popup, *point) == selected for point in disc + [ring]),
          "The selected root disc and full ring are not both visible")
    check(all(pixel(popup, *point) == neutral for point in leaves),
          "The children of the full ring are not painted neutral")
    check(not visible((1, 1)), "The popup corner outside the rings is visible")
    for point in disc + [ring]:
        wait_for_pixels(lambda: pixel(root, *screen_position(popup, *point)) == selected,
                        "Compositor did not display the root disc and the full ring")


try:
    command = [str(Path(__file__).resolve().parent / "project1"), "--test-clicks"]
    if args.dpi is not None:
        command.append(f"--test-dpi={args.dpi}")
    if args.captions:
        command.append("--test-captions")
    if args.full_ring:
        command.append("--test-full-ring")
    process = subprocess.Popen(
        command,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
    )
    host_report = wait_output_line("RADIAL_HOST ").split(" ", 4)
    host = [int(value) for value in host_report[:4]]
    main = wait_window(host_report[4])
    main_info = subprocess.check_output(["xwininfo", "-id", hex(main), "-stats"], text=True)
    main_size = [int(next(line.split(":")[1] for line in main_info.splitlines()
                         if line.strip().startswith(label))) for label in ("Width:", "Height:")]
    margin = min(host[2:]) // 10
    open_point = tuple(min(max(main_size[axis] // 2, host[axis] + margin),
                           host[axis] + host[axis + 2] - 1 - margin) for axis in (0, 1))
    title = "RadialTreeMenu"

    def open_menu():
        move(main, *open_point)
        check(main in pointer_chain(), "The own main form is not below the pointer")
        click(3)
        return wait_window(title)

    popup = open_menu()
    info = subprocess.check_output(["xwininfo", "-id", hex(popup), "-stats"], text=True)
    width = int(next(line.split(":")[1] for line in info.splitlines()
                     if line.strip().startswith("Width:")))
    kind = 2 if "Depth: 32" in info else 0
    scale, center = width / 344, width // 2
    if args.dpi is not None:
        check(width == 2 * round(172 * args.dpi / 96),
              f"Popup width {width} does not match requested DPI {args.dpi}")

    def point_at(radius, degrees):
        angle = math.radians(degrees)
        return (round(center + radius * scale * math.cos(angle)),
                round(center + radius * scale * math.sin(angle)))

    if args.full_ring:
        check_full_ring(popup, kind, point_at)
        if args.snapshot:
            save_snapshot(popup, width, kind == 2, args.snapshot)
        click(3)
        check(find(title) is None, "Right-click did not close the full-ring menu")
        check(not x_errors, f"Unexpected X11 errors (error code, request code): {x_errors}")
        print("PASS: the single root opens with the popup; the full ring and the root disc stay visible together")
        sys.exit(0)  # The finally block stops the demo and restores the pointer.

    palette = []

    def caption_free_probe(radius, degrees):
        candidates = [point_at(radius + 15, degrees + offset) for offset in (-10, 10)]
        label_y = point_at(radius, degrees)[1]
        return max(candidates, key=lambda point: abs(point[1] - label_y))

    def radial(radius, degrees):
        point = point_at(radius, degrees)
        move(popup, *point)
        if radius < 70:
            index = int(((degrees + 90) % 360) // 120)
            probe = point_at(50, (-90 + index * 120) + 90)
            if not palette:
                other = point_at(50, 90)
                wait_for_pixels(lambda: pixel(popup, *probe) != pixel(popup, *other),
                                "Root selection was not painted")
                palette.extend((pixel(popup, *probe), pixel(popup, *other)))
            children = [caption_free_probe(95, -60 + index * 120 + child * 60)
                        for child in range(2)]
            wait_for_pixels(
                lambda: pixel(popup, *probe) == palette[0] and
                all(pixel(popup, *child) == palette[1] for child in children),
                "Root/child selection state was not painted",
            )
        else:
            probe = caption_free_probe(radius, degrees)
            wait_for_pixels(lambda: pixel(popup, *probe) == palette[0],
                            "Child/leaf selection was not painted")
        return point

    def outside_click(point):
        # Stay above the own main form: without a grab the click must not reach other programs.
        sx, sy = screen_position(popup, *point)
        left, top = screen_position(main, 0, 0)
        check(left < sx < left + main_size[0] and top < sy < top + main_size[1],
              "Outside-click target is not above the own main form")
        check(not any(rx <= point[0] < rx + rw and ry <= point[1] < ry + rh
                      for rx, ry, rw, rh in shape(popup, kind)),
              "Outside-click target is inside the menu shape")
        move(popup, *point)
        check(main in pointer_chain(), "The own main form is not below the outside-click target")
        click(1)

    radial(40, -30)
    first_root = shape(popup, kind)
    check(pointer_grabbed(), "Open popup does not hold the pointer")
    click(1)
    check(find(title) == popup, "Clicking a root with children closed the menu")
    check(shape(popup, kind) == first_root, "Root click changed the visible branches")
    wait_for_pixels(pointer_grabbed, "Click inside the menu released the pointer grab")
    if args.snapshot and args.captions:
        save_snapshot(popup, width, kind == 2, args.snapshot)
    selected = pixel(popup, *point_at(50, 0))
    neutral = pixel(popup, *point_at(50, 90))
    check(neutral == pixel(popup, *point_at(50, 180)),
          "Unselected sectors do not share the neutral menu color")
    check(selected != neutral,
          f"Active sector has no selection color (active #{selected:06x}, inactive #{neutral:06x})")
    edge_x, edge_y = point_at(48, 30)
    edge_colors = {pixel(popup, px, py)
                   for py in range(edge_y - 5, edge_y + 6)
                   for px in range(edge_x - 5, edge_x + 6)} - {selected, neutral}
    check(len(edge_colors) >= 6, "Separator has insufficient antialiased coverage")
    check(0 not in edge_colors, "Separator is hard black rather than subtle")
    print(f"PASS: neutral/selected palette #{neutral:06x}/#{selected:06x}, "
          f"{len(edge_colors)} separator coverage colors")
    if args.captions:
        span = check_caption_lines(popup, center, scale, selected)
        print(f"CAPTION_METRICS dpi={args.dpi or 96} width={width} span={span} lines=2")
    sample = (round(center - 48 * scale), round(center - 82 * scale))
    root_probe = point_at(50, 0)
    wait_for_pixels(
        lambda: pixel(root, *screen_position(popup, *root_probe)) == selected,
        "Initial root selection was not composited",
    )
    background = pixel(root, *screen_position(popup, *sample))
    radial(40, 210)
    if args.captions:
        check_caption_clip(popup, center, scale, selected)
        print("PASS: long caption is clipped to its own segment")
    third_root = shape(popup, kind)
    child_point = radial(95, 240)
    descendants = shape(popup, kind)
    click(1)
    check(find(title) == popup, "Clicking a child with descendants closed the menu")
    check(shape(popup, kind) == descendants, "Inner child click changed the visible branches")
    leaf_point = radial(145, 240)
    check(shape(popup, kind) == descendants, "Leaf hover changed the ancestor layout")
    if args.snapshot and not args.captions:
        save_snapshot(popup, width, kind == 2, args.snapshot)
    for point in (child_point, leaf_point):
        wait_for_pixels(
            lambda: pixel(root, *screen_position(popup, *point)) == pixel(popup, *point),
            "Compositor did not display the child/leaf pixels",
        )
    check(area(descendants) > area(third_root), "Supplied grandchild did not add a ring")
    radial(40, -30)
    check(shape(popup, kind) == first_root, "Root switch retained obsolete descendants")
    wait_for_pixels(lambda: pixel(root, *screen_position(popup, *sample)) == background,
                    "Obsolete child remained visible on the screen")
    radial(95, -60)
    check(shape(popup, kind) == first_root, "Leaf-only branch opened a fabricated ring")
    for _ in range(3):
        radial(40, 210)
        radial(95, 240)
        check(shape(popup, kind) == descendants, "Repeated expansion changed the region")
        radial(95, 180)
        check(shape(popup, kind) == third_root, "Sibling switch retained a grandchild")
    outside_click(point_at(110, 0))
    check(find(title) is None, "Cutout click did not close the menu")
    check(not pointer_grabbed(), "Closed popup still holds the pointer")
    popup = open_menu()
    # Leave the popup bounds on the side facing the form center.
    side = 1 if open_point[0] < main_size[0] / 2 else -1
    outside_click((center + side * (center + round(15 * scale)), center))
    check(find(title) is None, "Outside-bounds click did not close the menu")
    popup = open_menu()
    move(popup, *point_at(45, 90))
    wait_for_pixels(lambda: pixel(popup, *point_at(50, 90)) == selected,
                    "Root-level leaf selection was not painted")
    click(1)
    check(find(title) is None, "Root-level leaf click did not close the menu")
    popup = open_menu()
    radial(40, -30)
    radial(95, -60)
    click(1)
    check(find(title) is None, "First-ring leaf click did not close the menu")
    popup = open_menu()
    radial(40, 210)
    radial(95, 240)
    radial(145, 240)
    click(1)
    check(find(title) is None, "Outer-ring leaf click did not close the menu")
    popup = open_menu()
    radial(40, 210)
    radial(95, 240)
    click(3)
    check(find(title) is None, "Right-click on an inner node did not close the menu")
    print("PASS: branch clicks preserve the popup, leaves close at every tested depth,")
    print("      and right-click/outside-click cancellation remains unchanged")
    stop_demo()
    memo = [line[len("MEMO_LINE "):] for line in output.splitlines()
            if line.startswith("MEMO_LINE ")]
    # The root-level leaf of the fixture has no handler; captions are empty with --captions.
    # The demo's tree-menu handler prefixes its lines with "Tree ".
    expected = ["Tree " + ("" if args.captions else caption) + " clicked"
                for caption in ("Subitem1.1", "Subitem3.2.1")]
    check(memo == expected, f"Demo memo received {memo} instead of {expected}")
    print("PASS: each handled leaf click appended exactly one line to the demo memo")
    check(not x_errors, f"Unexpected X11 errors (error code, request code): {x_errors}")
    print("PASS: caller-supplied tree hover, leaf-only branches, ancestor retention,")
    print("      branch replacement, screen pixels, repeated expansion and outside clicks")
finally:
    stop_demo()
    if output:
        print(output)
    move_screen(*saved_pointer)
    x.XCloseDisplay(d)
