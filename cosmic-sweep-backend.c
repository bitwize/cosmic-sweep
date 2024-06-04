/* Cosmic Sweep - Display Backend */

/* The display backend is the interface that does display and event
   handling on behalf of the Scheme game logic. It provides an
   interface to certain X11 primitives without having to have a full
   FFI to X11 in Scheme. */


#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <stdint.h>

#define ERR_NO_DISPLAY 1

#define WINDOW_WIDTH 720
#define WINDOW_HEIGHT 540

#define CS_KEY_LEFT 1
#define CS_KEY_RIGHT 2
#define CS_KEY_THRUST 4
#define CS_KEY_FIRE 8

#define CS_COLOR_BLACK 0
#define CS_COLOR_RED 1
#define CS_COLOR_YELLOW 2
#define CS_COLOR_GREEN 3
#define CS_COLOR_CYAN 4
#define CS_COLOR_BLUE 5
#define CS_COLOR_MAGENTA 6
#define CS_COLOR_WHITE 7
#define CS_NCOLORS 8

#define DLIST_MAX 
unsigned long cs_colors[CS_NCOLORS];

Display *dpy;
Window w;
int screen;
Colormap cmap;
Pixmap back_buf;
unsigned long black, white;
unsigned int key_status;
GC window_gc, back_gc;

int init_window() {
	XColor xcol;
	XGCValues gcv;
	dpy = XOpenDisplay(NULL);
	if(dpy == NULL) {
		return ERR_NO_DISPLAY;
	}
	screen = DefaultScreen(dpy);
	black = BlackPixel(dpy, screen);
	white = WhitePixel(dpy, screen);
	cmap = DefaultColormap(dpy, screen);
	cs_colors[CS_COLOR_BLACK] = black;
	cs_colors[CS_COLOR_WHITE] = white;
	xcol.red = 0xff00;
	xcol.green = 0;
	xcol.blue = 0;
	XAllocColor(dpy, cmap, &xcol);
	cs_colors[CS_COLOR_RED] = xcol.pixel;
	xcol.red = 0xff00;
	xcol.green = 0xff00;
	xcol.blue = 0;
	XAllocColor(dpy, cmap, &xcol);
	cs_colors[CS_COLOR_YELLOW] = xcol.pixel;
	xcol.red = 0;
	xcol.green = 0xff00;
	xcol.blue = 0;
	XAllocColor(dpy, cmap, &xcol);
	cs_colors[CS_COLOR_GREEN] = xcol.pixel;
	xcol.red = 0;
	xcol.green = 0xff00;
	xcol.blue = 0xff00;
	XAllocColor(dpy, cmap, &xcol);
	cs_colors[CS_COLOR_CYAN] = xcol.pixel;
	xcol.red = 0;
	xcol.green = 0;
	xcol.blue = 0xff00;
	XAllocColor(dpy, cmap, &xcol);
	cs_colors[CS_COLOR_BLUE] = xcol.pixel;
	xcol.red = 0xff00;
	xcol.green = 0;
	xcol.blue = 0xff00;
	XAllocColor(dpy, cmap, &xcol);
	cs_colors[CS_COLOR_MAGENTA] = xcol.pixel;
	w = XCreateSimpleWindow(dpy,
				DefaultRootWindow(dpy),
				0,
				0,
				WINDOW_WIDTH,
				WINDOW_HEIGHT,
				0,
				black,
				black);
	XSelectInput(dpy,
		     w,
		     KeyPressMask |
		     KeyReleaseMask |
		     ExposureMask);
	XMapWindow(dpy, w);
	back_buf = XCreatePixmap(dpy,
				 w,
				 WINDOW_WIDTH,
				 WINDOW_HEIGHT,
				 DefaultDepth(dpy, screen));
	gcv.foreground = black;
	gcv.function = GXcopy;
	gcv.fill_style = FillSolid;
	window_gc = XCreateGC(dpy,
			      w,
			      GCForeground | GCFunction | GCFillStyle,
			      &gcv);
	gcv.foreground = white;
	back_gc = XCreateGC(dpy,
			      back_buf,
			      GCForeground | GCFunction | GCFillStyle,
			      &gcv);
	return 0;
}

void show_backbuf() {
	XCopyArea(dpy, back_buf, w, window_gc, 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT, 0, 0);
	XFlush(dpy);
}

void process_events() {
	XEvent e;
	KeySym ks;
	while(XPending(dpy)) {
		XNextEvent(dpy, &e);
		switch(e.type) {
		case KeyPress:
			ks = XLookupKeysym(&(e.xkey), 0);
			switch(ks) {
			case XK_Up:
				key_status |= CS_KEY_THRUST;
				break;
			case XK_Left:
				key_status |= CS_KEY_LEFT;
				break;
			case XK_Right:
				key_status |= CS_KEY_RIGHT;
				break;
			case XK_space:
				key_status |= CS_KEY_FIRE;
				break;
			}
			break;
		case KeyRelease:
			ks = XLookupKeysym(&(e.xkey), 0);
			switch(ks) {
			case XK_Up:
				key_status &= ~CS_KEY_THRUST;
				break;
			case XK_Left:
				key_status &= ~CS_KEY_LEFT;
				break;
			case XK_Right:
				key_status &= ~CS_KEY_RIGHT;
				break;
			case XK_space:
				key_status &= ~CS_KEY_FIRE;
				break;
			}
			break;
		case Expose:
			show_backbuf();
			break;
		}
	}
}

unsigned int poll_keys() {
	return key_status;
}

void clear_screen() {
	XGCValues gcv;
	gcv.foreground = cs_colors[0];
	XChangeGC(dpy, back_gc, GCForeground, &gcv);
	XFillRectangle(dpy, back_buf, back_gc, 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT);
	XFlush(dpy);
}

void draw_pline(uint32_t *values, size_t length, unsigned int color) {
	unsigned int real_color = color % CS_NCOLORS;
	unsigned int i;
	XGCValues gcv;
	short x1, y1, x2, y2;

	if(length < 2) {
		return;
	}
	gcv.foreground = cs_colors[real_color];
	XChangeGC(dpy, back_gc, GCForeground, &gcv);
	x1 = (short)(values[0] >> 16) & ((short)0xffff);
	y1 = (short)(values[0]) & ((short)0xffff);
	for(i = 1; i < length; i++) {
		x2 = (short)(values[i] >> 16) & ((short)0xffff);
		y2 = (short)(values[i]) & ((short)0xffff);
		XDrawLine(dpy, back_buf, back_gc, x1, y1, x2, y2);
		x1 = x2; y1 = y2;
	}
	XFlush(dpy);
}
