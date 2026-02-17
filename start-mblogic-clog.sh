#!/usr/bin/env bash
# start-mblogic-clog.sh — Start/stop the MBLogic-CLOG web server

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PIDFILE="$SCRIPT_DIR/.mblogic-clog.pid"
LOGFILE="$SCRIPT_DIR/mblogic-clog.log"
GENERIC_SCRIPT="$SCRIPT_DIR/start-web-server-generic.lisp"

# Optional second argument: IL file path (relative to project root or absolute)
# Default: test/demodataprog.txt
IL_ARG="${2:-}"
if [ -z "$IL_ARG" ]; then
    IL_FILE="$SCRIPT_DIR/test/demodataprog.txt"
elif [[ "$IL_ARG" = /* ]]; then
    IL_FILE="$IL_ARG"
else
    IL_FILE="$SCRIPT_DIR/$IL_ARG"
fi

usage() {
    echo "Usage: $0 {start|stop|restart|status} [IL_FILE]"
    echo ""
    echo "  start [FILE]  Start the web server with optional IL program file"
    echo "  stop          Stop the web server"
    echo "  restart [FILE] Stop then start"
    echo "  status        Show whether the server is running"
    echo ""
    echo "  FILE: path to IL program (relative to project root or absolute)"
    echo "        default: test/demodataprog.txt"
    echo ""
    echo "  Examples:"
    echo "    $0 start"
    echo "    $0 start test/plcprog.txt"
    echo "    $0 start /path/to/myprogram.txt"
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

    echo "Starting MBLogic-CLOG web server with: $IL_FILE"
    sbcl --eval "(defvar *il-file* \"$IL_FILE\")" \
         --load "$GENERIC_SCRIPT" >> "$LOGFILE" 2>&1 &
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
