@echo off
REM Install MSYS2
echo Installing MSYS2 and dependencies...
curl -LO https://github.com/msys2/msys2-installer/releases/download/2024-11-16/msys2-x86_64-20241116.exe
start /wait msys2-x86_64-20241116.exe --quiet-mode

REM Verify MSYS2 installation
IF NOT EXIST "C:\msys64" (
    echo MSYS2 installation failed. Exiting...
    exit /b 1
)

REM Update MSYS2 packages
echo Updating MSYS2 packages...
C:\msys64\usr\bin\bash -lc "pacman -Syu --noconfirm  exit 1"

REM Install GTK+3 and dependencies
echo Installing GTK+3...
C:\msys64\usr\bin\bash -lc "pacman -S --noconfirm mingw-w64-ucrt-x86_64-gtk3 mingw-w64-ucrt-x86_64-pkg-config  exit 1"

REM Add GTK+3 to PKG_CONFIG_PATH
echo Setting up PKG_CONFIG_PATH...
setx PKG_CONFIG_PATH "C:\msys64\ucrt64\lib\pkgconfig;%PKG_CONFIG_PATH%"

REM Verify GTK+3 installation
echo Verifying GTK+3 installation...
C:\msys64\usr\bin\bash -lc "pkg-config --modversion gtk+-3.0 || exit 1"

REM Install Haskell Stack
echo Installing Haskell Stack...
curl -sSL https://get.haskellstack.org/ | C:\msys64\ucrt64\bin\bash

REM Run stack setup and build
echo Running stack setup and build...
stack setup
stack build