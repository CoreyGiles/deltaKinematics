# deltaKinematics

Alternative to cartesian 3D printers are steadily growing. While 32 bit microcontrollers are becoming cheaper and more popular, 8 bit microcontrollers are still the dominant processors in many 3D printer boards.

This code is based on the teacup firmware: https://github.com/Traumflug/Teacup_Firmware

Most algorithms split each G-Code movement into smaller movements, which are then converted into delta robot movements. Currently, no efficient algorithm exists that can be performed on an 8 bit mcu for controlling a delta robot directly.

This work explores the use of partial derivatives to convert cartesian velocity (speed and direction) into delta velocity. Temporal acceleration is used for sequencing motor steps.

