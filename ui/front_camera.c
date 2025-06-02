#include <gtk/gtk.h>        // GTK UI
#include <opencv2/opencv.hpp> // OpenCV C++ API
#include <opencv2/highgui/highgui_c.h> // If using OpenCV C API
#include <opencv2/imgproc/imgproc_c.h> // If needed
#include <stdlib.h>          // For NULL
#include <stdio.h>           // For standard IO if needed


// Global GTK widgets
GtkWidget *image_widget;
cv::VideoCapture cap;
GdkPixbuf *pixbuf = NULL;

// Function to update the image widget from OpenCV frame
gboolean update_frame(gpointer user_data) {
    cv::Mat frame;
    if (!cap.read(frame)) {
        g_print("Failed to capture frame\n");
        return TRUE;
    }

    // Convert OpenCV BGR frame to RGB
    cv::cvtColor(frame, frame, cv::COLOR_BGR2RGB);

    // Create GdkPixbuf from frame data
    if (pixbuf)
        g_object_unref(pixbuf);
    pixbuf = gdk_pixbuf_new_from_data(
        frame.data,
        GDK_COLORSPACE_RGB,
        FALSE,
        8,
        frame.cols,
        frame.rows,
        frame.step,
        NULL,
        NULL
    );

    // Set the pixbuf on the GTK image widget
    gtk_image_set_from_pixbuf(GTK_IMAGE(image_widget), pixbuf);

    return TRUE; // Continue calling this function
}

int main(int argc, char *argv[]) {
    gtk_init(&argc, &argv);

    // Open front camera (usually device 0)
    cap.open(0);
    if (!cap.isOpened()) {
        g_print("Error: Could not open front camera\n");
        return -1;
    }

    // Create GTK window
    GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "Front Camera UI");
    gtk_window_set_default_size(GTK_WINDOW(window), 640, 480);

    // Create image widget to show camera frames
    image_widget = gtk_image_new();
    gtk_container_add(GTK_CONTAINER(window), image_widget);

    // Connect window destroy signal
    g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);

    // Add a timer to update frames every 30ms (~33 FPS)
    g_timeout_add(30, update_frame, NULL);

    gtk_widget_show_all(window);
    gtk_main();

    // Cleanup
    cap.release();
    if (pixbuf)
        g_object_unref(pixbuf);

    return 0;
}
