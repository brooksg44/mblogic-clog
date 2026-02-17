#!/usr/bin/env bash
# start-mblogic-clog.sh — Start/stop the MBLogic-CLOG web server

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PIDFILE="$SCRIPT_DIR/.mblogic-clog.pid"
LOGFILE="$SCRIPT_DIR/mblogic-clog.log"
STARTUP_SCRIPT="$SCRIPT_DIR/start-web-server-demo.lisp"

usage() {
    echo "Usage: $0 {start|stop|restart|status}"
    echo ""
    echo "  start    Start the web server (http://localhost:8080)"
    echo "  stop     Stop the web server"
    echo "  restart  Stop then start"
    echo "  status   Show whether the server is running"
    exit 1
}

is_running() {
    if [ -f "$PIDFILE" ]; then
        local pid
        pid=$(cat "$PIDFILE")
        if kill -0 "$pid" 2>/dev/null; then
            return 0
        fi
    fi
    return 1
}

cmd_start() {
    if is_running; then
        echo "MBLogic-CLOG is already running (PID $(cat "$PIDFILE"))"
        exit 0
    fi

    echo "Starting MBLogic-CLOG web server..."
    sbcl --load "$STARTUP_SCRIPT" >> "$LOGFILE" 2>&1 &
    local pid=$!
    echo "$pid" > "$PIDFILE"

    # Wait briefly and confirm it started
    sleep 3
    if kill -0 "$pid" 2>/dev/null; then
        echo "Started (PID $pid)"
        echo "  Browser: http://localhost:8080"
        echo "  Log:     $LOGFILE"
    else
        echo "Failed to start — check $LOGFILE"
        rm -f "$PIDFILE"
        exit 1
    fi
}

cmd_stop() {
    if ! is_running; then
        echo "MBLogic-CLOG is not running"
        rm -f "$PIDFILE"
        exit 0
    fi

    local pid
    pid=$(cat "$PIDFILE")
    echo "Stopping MBLogic-CLOG (PID $pid)..."
    kill "$pid"

    # Wait up to 5 seconds for clean exit
    local i=0
    while kill -0 "$pid" 2>/dev/null && [ $i -lt 5 ]; do
        sleep 1
        i=$((i + 1))
    done

    if kill -0 "$pid" 2>/dev/null; then
        kill -9 "$pid"
        echo "Force-killed"
    else
        echo "Stopped"
    fi

    rm -f "$PIDFILE"
}

cmd_status() {
    if is_running; then
        echo "MBLogic-CLOG is running (PID $(cat "$PIDFILE"))"
        echo "  Browser: http://localhost:8080"
    else
        echo "MBLogic-CLOG is not running"
    fi
}

case "${1:-}" in
    start)   cmd_start   ;;
    stop)    cmd_stop    ;;
    restart) cmd_stop; cmd_start ;;
    status)  cmd_status  ;;
    *)       usage       ;;
esac
