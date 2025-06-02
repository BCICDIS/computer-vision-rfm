import subprocess
import os

def call_main_exe():
    """
    Call the main.exe executable located in the bin directory.
    Returns the process return code.
    """
    exe_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'bin', 'main.exe')
    try:
        result = subprocess.run([exe_path], capture_output=True, text=True, encoding='utf-8', errors='replace')
        print(f"STDOUT: {result.stdout}")
        print(f"STDERR: {result.stderr}")
        return result.returncode
    except Exception as e:
        print(f"Error calling main.exe: {str(e)}")
        return -1

if __name__ == "__main__":
    call_main_exe()