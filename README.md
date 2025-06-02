# BlackVision: Computer Vison Framework
A built-in computer vision framework developed for [K.E.N.D.R.A.H-RA1018](https://github.com/BCICDIS/K.E.N.D.R.A.H-RA1018), designed to support real-time object detection, screen capture analysis, and camera-based recognition using modular integrations in C, Ada, and Python. This framework provides the core visual processing capabilities for operator dashboards, system monitoring, and intelligent vision-assisted automation.

![BlackVision - Computer Vision Framework](https://img.shields.io/badge/BlackVision-Computer%20Vision%20Framework-black?style=for-the-badge&logo=eye&logoColor=white)
![Owner](https://img.shields.io/badge/Owner-Black%20Corp%20Intelligence-blue?style=flat-square)
![License](https://img.shields.io/badge/License-Proprietary-red?style=flat-square)
![Category](https://img.shields.io/badge/Category-Computer%20Vision-green?style=flat-square)


# Diagram
```mermaid
flowchart TB
    %% Input Sources
    subgraph "Input Sources"
        direction TB
        Camera["Camera (Back/Front)"]:::input
        Screen["Screen Capture"]:::input
    end

    %% Python Methods Layer
    subgraph "Python Methods Layer"
        direction TB
        BackCam["back_camera.py"]:::python
        FrontCam["front_camera.py"]:::python
        ScreenMeth["screen_vision.py"]:::python
    end

    %% Python CV Frameworks
    subgraph "Python CV Frameworks"
        direction TB
        Detectron2["Detectron2.py"]:::python
        TensorFlow["TensorFlow.py"]:::python
        Pytorch["Pytorch.py"]:::python
        OpenCV["OpenCV.py"]:::python
        OpenVINO["OpenVINO.py"]:::python
    end

    %% Post-processing Utilities
    subgraph "Post-processing Utilities"
        direction TB
        AABB["aabb.py"]:::python
        Oriented["oriented_bounding_box.py"]:::python
        Box3D["3d_B_box.py"]:::python
        Polygon["B_polygon.py"]:::python
        MER["m_e_r.py"]:::python
    end

    %% Ada Core Engine
    subgraph "Ada Core Engine"
        direction TB
        MainAda["main.adb"]:::ada
        UtilsAdb["utils.adb"]:::ada
        UtilsAds["utils.ads"]:::ada
        GP["default.gpr"]:::ada
    end

    %% C UI Layer
    subgraph "C UI Layer"
        direction TB
        FrontUI["front_camera.c"]:::cui
        MainCUI["cv.ui.main.c"]:::cui
    end

    %% Executable Artifacts
    subgraph "Executable Artifacts"
        direction TB
        Bin["main.exe (bin/)"]:::external
        Obj["obj/main.o"]:::external
    end

    %% External Dependencies & Hardware
    subgraph "External Dependencies & Hardware"
        direction TB
        TFExt["TensorFlow"]:::external
        CVLib["OpenCV"]:::external
        CUDA["Nvidia CUDA"]:::external
        AdaRT["Ada Runtime"]:::external
        GPU["GPU"]:::external
        CPU["CPU"]:::external
    end

    %% Connections
    Camera --> BackCam
    Camera --> FrontCam
    Screen --> ScreenMeth

    BackCam --> Detectron2
    FrontCam --> Pytorch
    ScreenMeth --> OpenVINO

    Detectron2 --> AABB
    TensorFlow --> AABB
    Pytorch --> AABB
    OpenCV --> AABB
    OpenVINO --> AABB

    AABB --> MainAda
    Oriented --> MainAda
    Box3D --> MainAda
    Polygon --> MainAda
    MER --> MainAda

    MainAda --> FrontUI
    MainAda --> MainCUI

    FrontUI --> Bin
    MainCUI --> Bin
    MainAda --> Obj

    Detectron2 --> TFExt
    TensorFlow --> TFExt
    OpenCV --> CVLib
    Detectron2 --> CUDA
    Pytorch --> CUDA

    MainAda --> AdaRT
    FrontUI --> CPU
    TFExt --> GPU

    %% Click Events
    click BackCam "https://github.com/bcicdis/computer-vision-rfm/blob/main/methods/back_camera.py"
    click FrontCam "https://github.com/bcicdis/computer-vision-rfm/blob/main/methods/front_camera.py"
    click ScreenMeth "https://github.com/bcicdis/computer-vision-rfm/blob/main/methods/screen_vision.py"

    click Detectron2 "https://github.com/bcicdis/computer-vision-rfm/blob/main/frameworks/Detectron2.py"
    click TensorFlow "https://github.com/bcicdis/computer-vision-rfm/blob/main/frameworks/TensorFlow.py"
    click Pytorch "https://github.com/bcicdis/computer-vision-rfm/blob/main/frameworks/Pytorch.py"
    click OpenCV "https://github.com/bcicdis/computer-vision-rfm/blob/main/frameworks/OpenCV.py"
    click OpenVINO "https://github.com/bcicdis/computer-vision-rfm/blob/main/frameworks/OpenVINO.py"

    click AABB "https://github.com/bcicdis/computer-vision-rfm/blob/main/modules/boundingBox/aabb.py"
    click Oriented "https://github.com/bcicdis/computer-vision-rfm/blob/main/modules/boundingBox/oriented_bounding_box.py"
    click Box3D "https://github.com/bcicdis/computer-vision-rfm/blob/main/modules/boundingBox/3d_B_box.py"
    click Polygon "https://github.com/bcicdis/computer-vision-rfm/blob/main/modules/boundingBox/B_polygon.py"
    click MER "https://github.com/bcicdis/computer-vision-rfm/blob/main/modules/boundingBox/m_e_r.py"

    click MainAda "https://github.com/bcicdis/computer-vision-rfm/blob/main/src/main.adb"
    click UtilsAdb "https://github.com/bcicdis/computer-vision-rfm/blob/main/src/utils.adb"
    click UtilsAds "https://github.com/bcicdis/computer-vision-rfm/blob/main/src/utils.ads"
    click GP "https://github.com/bcicdis/computer-vision-rfm/blob/main/default.gpr"

    click FrontUI "https://github.com/bcicdis/computer-vision-rfm/blob/main/ui/front_camera.c"
    click MainCUI "https://github.com/bcicdis/computer-vision-rfm/blob/main/cv.ui.main.c"

    click Bin "https://github.com/bcicdis/computer-vision-rfm/blob/main/bin/main.exe"
    click Obj "https://github.com/bcicdis/computer-vision-rfm/blob/main/obj/main.o"

    %% Styles
    classDef input fill:#ccffcc,stroke:#006600
    classDef python fill:#cce5ff,stroke:#004085
    classDef ada fill:#ffe5cc,stroke:#dc3545
    classDef cui fill:#f0ccff,stroke:#800080
    classDef external fill:#e2e3e5,stroke:#6c757d
```
