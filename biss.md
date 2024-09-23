# BiSS-C Encoder Controller with APB3 Interface

## Table of Contents
- [Overview](#overview)
- [Features](#features)
- [System Requirements](#system-requirements)
- [Design Architecture](#design-architecture)
- [APB3 Register Mapping](#apb3-register-mapping)
- [Usage Instructions](#usage-instructions)
- [Testing and Validation](#testing-and-validation)
- [Further Enhancements](#further-enhancements)
- [License](#license)

## Overview

The **BiSS-C Encoder Controller** is a robust and flexible hardware component designed using **SpinalHDL**. It interfaces with **BiSS-C** (Bidirectional Serial Synchronous) absolute encoders, providing multi-turn position tracking with support for various resolutions and configurable communication speeds. The controller communicates with a host system via an **APB3 (Advanced Peripheral Bus 3)** interface, allowing seamless integration into larger digital systems.

## Features

- **Multi-Turn Support**: Tracks both positive and negative rotation counts using a signed 32-bit integer.
- **Variable Resolutions**: Supports 18-bit, 26-bit, 32-bit, and 36-bit encoder resolutions.
- **Configurable Speeds**: Operates at 1 MHz, 2 MHz, 5 MHz, and 10 MHz data reception speeds.
- **CRC Validation**: Ensures data integrity with 6-bit CRC checks.
- **Over-Run Detection**: Monitors and flags counter overflows and underflows.
- **APB3 Interface**: Provides a comprehensive register map for configuration and status monitoring.
- **Modular Design**: Utilizes SpinalHDL's advanced features and libraries for maintainability and scalability.

## System Requirements

- **SpinalHDL**: Version compatible with the provided code (ensure latest stable release).
- **Scala**: Required for running SpinalHDL.
- **FPGA Development Environment**: To synthesize and implement the design on an FPGA (e.g., Xilinx Vivado, Intel Quartus).
- **BiSS-C Compatible Encoder**: For hardware testing and validation.

## Design Architecture

The project is structured into several key components:

1. **Enumerations and Helpers**:
   - `BisscResolution`: Defines supported encoder resolutions.
   - `BisscResolutionHelper`: Provides utility functions for resolution management.

2. **Generics and Configurations**:
   - `BisscGenerics`: Encapsulates generic parameters like resolution bits and CRC width.
   - `BisscSlaveCtrlMemoryMappedConfig`: Holds generic configurations for the slave controller.

3. **BiSS-C Interface**:
   - `BissCInterface`: Defines the BiSS-C master-slave signals (`ma` and `slo`).

4. **APB3 Slave Controller**:
   - `Apb3BisscSlaveCtrl`: Integrates the BiSS-C controller with the APB3 bus, handling register mappings and interrupt logic.

5. **BiSS-C Slave Controller Logic**:
   - `BisscSlaveCtrl`: Manages core functionalities, interfacing with the receiver and exposing signals via APB3.

6. **BiSS-C Receiver**:
   - `BissCReceiver`: Handles data reception, parsing position and turn count information, computing CRC, and detecting over-run conditions.

7. **CRC6 Computation**:
   - `CRCGenerator`: Utilizes SpinalHDL's CRC library for reliable CRC calculations.

8. **Testbench Simulation**:
   - `Apb3BisscSlaveCtrlSim`: Simulates various scenarios to validate the controller's functionality across different resolutions and rotation directions.

## APB3 Register Mapping

The controller exposes several registers via the APB3 interface for configuration and status monitoring. Below is the detailed register mapping:

| Register Name            | Address (Hex) | Read/Write | Description                                             |
|--------------------------|---------------|------------|---------------------------------------------------------|
| **Position Data**        | `0x00`        | Read-only  | 32/36-bit position data                                 |
| **Error Status**         | `0x04`        | Read-only  | Error flag                                              |
| **Warning Status**       | `0x08`        | Read-only  | Warning flag                                            |
| **CRC Error Status**     | `0x0C`        | Read-only  | CRC error flag                                          |
| **Reset Control**        | `0x10`        | Write-only | Resets the controller                                   |
| **Start Request**        | `0x14`        | Write-only | Initiates data reception                                 |
| **Speed Selection**      | `0x18`        | Write-only | Configures data reception speed                          |
| **Resolution Selection** | `0x1C`        | Write-only | Configures encoder resolution (18, 26, 32, 36-bit)       |
| **Interrupt Status**     | `0x20`        | Read-only  | Indicates data ready or error conditions                 |
| **Turn Count**           | `0x24`        | Read-only  | 32-bit signed integer representing the number of completed turns |
| **Over-Run Status**      | `0x28`        | Read-only  | Indicates if Turn Count has exceeded its signed range     |

### Register Descriptions

- **Position Data (`0x00`)**: Provides the current position from the encoder. Supports multiple resolutions.
  
- **Error Status (`0x04`)**: Flags any errors encountered during data reception.
  
- **Warning Status (`0x08`)**: Signals warning conditions that may require attention.
  
- **CRC Error Status (`0x0C`)**: Indicates a CRC mismatch, ensuring data integrity.
  
- **Reset Control (`0x10`)**: Writing to this register resets the controller to its initial state.
  
- **Start Request (`0x14`)**: Initiates the data reception process when written to.
  
- **Speed Selection (`0x18`)**: Configures the data reception speed. Values:
  - `0`: 1 MHz
  - `1`: 2 MHz
  - `2`: 5 MHz
  - `3`: 10 MHz
  
- **Resolution Selection (`0x1C`)**: Sets the encoder resolution. Values:
  - `0`: 18-bit
  - `1`: 26-bit
  - `2`: 32-bit
  - `3`: 36-bit
  
- **Interrupt Status (`0x20`)**: Reflects interrupt conditions such as data readiness or errors.
  
- **Turn Count (`0x24`)**: Represents the number of completed turns since the system started or last reset. It's a signed 32-bit integer (`SInt`), allowing both positive and negative counts based on rotation direction.
  
- **Over-Run Status (`0x28`)**: Flags when the Turn Count reaches its maximum (`+0x7FFFFFFF`) or minimum (`-0x80000000`) values, preventing counter overflows or underflows.

## Usage Instructions

### 1. **Initialization**
   
   - **Reset the Controller**:
     - Write `1` to the **Reset Control** register (`0x10`).
     - The controller will reset all internal registers and state machines.
   
### 2. **Configuration**

   - **Set Resolution**:
     - Write to the **Resolution Selection** register (`0x1C`) with the desired resolution code:
       - `0`: 18-bit
       - `1`: 26-bit
       - `2`: 32-bit
       - `3`: 36-bit
  
   - **Set Speed**:
     - Write to the **Speed Selection** register (`0x18`) with the desired speed code:
       - `0`: 1 MHz
       - `1`: 2 MHz
       - `2`: 5 MHz
       - `3`: 10 MHz
  
### 3. **Data Reception**

   - **Start Receiving Data**:
     - Write `1` to the **Start Request** register (`0x14`) to initiate data reception.
     - The controller will begin capturing incoming BiSS-C frames based on the configured resolution and speed.

### 4. **Monitoring and Validation**

   - **Read Position Data**:
     - Read from the **Position Data** register (`0x00`) to obtain the current position.
  
   - **Read Turn Count**:
     - Read from the **Turn Count** register (`0x24`) to get the number of completed turns.
  
   - **Check Over-Run Status**:
     - Read from the **Over-Run Status** register (`0x28`) to detect if the Turn Count has exceeded its signed range.
  
   - **Monitor Error Flags**:
     - **Error Status (`0x04`)**, **Warning Status (`0x08`)**, and **CRC Error Status (`0x0C`)** should be periodically checked to ensure reliable operation.
  
   - **Handle Interrupts**:
     - The **Interrupt Status** register (`0x20`) can be used to handle data readiness or error conditions in an interrupt-driven manner.

### 5. **Error Handling**

   - **CRC Errors**:
     - If the **CRC Error Status** (`0x0C`) is set, verify data integrity and consider initiating a reset or requesting retransmission.
  
   - **Over-Run Conditions**:
     - When **Over-Run Status** (`0x28`) is asserted, take appropriate actions such as resetting the Turn Count or alerting the system to prevent inaccuracies.

## Testing and Validation

A comprehensive testbench (`Apb3BisscSlaveCtrlSim`) is provided to simulate various scenarios and validate the controller's functionality. The testbench covers:

- **Multiple Resolutions**: Ensures accurate position and turn count tracking across 18-bit, 26-bit, 32-bit, and 36-bit resolutions.
- **Bidirectional Rotation**: Tests both positive and negative turn counts to validate signed integer handling.
- **Over-Run Scenarios**: Simulates conditions where the Turn Count approaches its maximum and minimum limits to verify over-run detection.
- **CRC Validation**: Confirms that CRC computations and error flagging operate correctly.

### Running the Testbench

1. **Setup**:
   - Ensure SpinalHDL and Scala are properly installed.
   - Navigate to the project directory containing the SpinalHDL source files.

2. **Execute Simulation**:
   - Run the testbench using SBT or your preferred Scala build tool:
     ```bash
     sbt "runMain Apb3BisscSlaveCtrlSim"
     ```
  
3. **Analyze Results**:
   - The simulation will print out the results of each test case, including position, turn count, and status flags.
   - Assertions within the testbench will validate the correctness of each scenario, flagging any discrepancies.

## Further Enhancements

While the current design is robust and feature-complete for multi-turn BiSS-C encoders, the following enhancements can be considered for future iterations:

1. **Bi-Directional Communication**:
   - Implementing the ability to send commands or configurations back to the encoder if supported by the hardware.

2. **Advanced Error Recovery**:
   - Developing mechanisms to automatically recover from detected errors, such as CRC mismatches or over-run conditions.

3. **Extended Protocol Support**:
   - Incorporating additional BiSS-C protocol features or supporting other encoder communication protocols for broader compatibility.

4. **Power Optimization**:
   - Implementing power-saving features like clock gating when the controller is idle to reduce energy consumption.

5. **User-Friendly Interfaces**:
   - Developing higher-level drivers or software interfaces that abstract away low-level register manipulations for easier integration into software applications.

6. **Hardware Validation**:
   - Deploying the design on actual FPGA hardware and conducting real-world tests with BiSS-C encoders to validate performance and reliability.

## License

This project is licensed under the [MIT License](LICENSE).

---
