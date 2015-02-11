#-------------------------------------------------
#
# Project created by QtCreator 2014-12-16T13:16:40
#
#-------------------------------------------------

QT       -= core gui

TARGET = cppop
TEMPLATE = lib

DEFINES += CPPOP_LIBRARY

SOURCES += \
    server.cpp \
    interface.cpp \
    driver.cpp \
    message.cpp \
    common.cpp \
    buffer.cpp

HEADERS +=\
    server.h \
    interface.h \
    driver.h \
    common.h \
    idspace.h \
    mvar.h \
    message.h \
    buffer.h

LIBS += -lboost_thread

unix:!symbian {
    maemo5 {
        target.path = /opt/usr/lib
    } else {
        target.path = /usr/lib
    }
    INSTALLS += target
}
