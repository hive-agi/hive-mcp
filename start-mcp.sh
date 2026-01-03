#!/bin/bash
# Start the Emacs MCP server
# This script is called by Claude's MCP configuration

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Ensure emacsclient is available
if ! command -v emacsclient &> /dev/null; then
    echo "Error: emacsclient not found in PATH" >&2
    exit 1
fi

# Check if Emacs server is running
if ! emacsclient -e "t" &> /dev/null; then
    echo "Warning: Emacs server not running. Start Emacs with (server-start)" >&2
fi

cd "$SCRIPT_DIR"

# Chroma configuration - override via environment or Emacs config
# These can be set in your shell profile, Emacs config, or systemd unit
export CHROMA_HOST="${CHROMA_HOST:-localhost}"
export CHROMA_PORT="${CHROMA_PORT:-8000}"
export OLLAMA_HOST="${OLLAMA_HOST:-http://localhost:11434}"

# Channel port for bidirectional Emacs communication
export EMACS_MCP_CHANNEL_PORT="${EMACS_MCP_CHANNEL_PORT:-9998}"

# nREPL port - embedded in MCP server for bb-mcp tool forwarding
export EMACS_MCP_NREPL_PORT="${EMACS_MCP_NREPL_PORT:-7910}"

# Kill any stale processes on our ports before starting
# The embedded nREPL MUST run in the same JVM as the channel server
# so hivemind broadcasts actually reach connected clients
fuser -k "${EMACS_MCP_CHANNEL_PORT}/tcp" 2>/dev/null || true
fuser -k "${EMACS_MCP_NREPL_PORT}/tcp" 2>/dev/null || true

# Run the MCP server
exec clojure -X:mcp
