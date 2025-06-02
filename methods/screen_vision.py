import cv2
import numpy as np
import pyautogui
import datetime
import os

# Define path
shared_dir = os.path.join(".", "shared")
log_file = os.path.join(shared_dir, "output_log.txt")

# ✅ Make sure the shared directory exists
os.makedirs(shared_dir, exist_ok=True)

# Create the log file if it doesn't exist
if not os.path.exists(log_file):
    with open(log_file, "w") as f:
        f.write("Log file created\n")

def write_log(message):
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    with open(log_file, "a") as f:
        f.write(f"[{timestamp}] {message}\n")

def process_frame(frame):
    # Example: detect light-colored regions
    gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
    _, thresh = cv2.threshold(gray, 200, 255, cv2.THRESH_BINARY)

    # Example: trigger log if many white pixels exist
    white_pixels = cv2.countNonZero(thresh)
    if white_pixels > 50000:
        write_log("Bright screen region detected.")

    # Show processing
    cv2.imshow("Processed", thresh)

def main():
    print("[INFO] Starting screen vision (press 'q' to quit)")
    write_log("Screen capture started")

    try:
        while True:
            screenshot = pyautogui.screenshot()
            frame = np.array(screenshot)
            frame = cv2.cvtColor(frame, cv2.COLOR_RGB2BGR)

            # Show original screen
            cv2.imshow("Live Screen", frame)

            # Process frame
            process_frame(frame)

            if cv2.waitKey(1) & 0xFF == ord('q'):
                write_log("User quit screen capture")
                break

    except KeyboardInterrupt:
        write_log("Capture interrupted by user")

    finally:
        cv2.destroyAllWindows()

if __name__ == "__main__":
    main()
