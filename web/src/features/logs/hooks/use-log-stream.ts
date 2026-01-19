import { useCallback, useEffect, useRef, useState } from "react";
import type { LogEntry, LogLevel } from "@/lib/api";

export interface UseLogStreamParams {
  enabled?: boolean;
  levelFilter?: LogLevel | null;
  onMessage?: (entry: LogEntry) => void;
  onError?: (error: Event) => void;
}

/**
 * SSE 实时日志推送 Hook
 *
 * 连接 /api/logs/stream 端点,接收实时日志流
 * 支持客户端级别过滤、自动重连、错误处理
 */
export function useLogStream(params: UseLogStreamParams = {}) {
  const { enabled = false, levelFilter = null, onMessage, onError } = params;
  const [entries, setEntries] = useState<LogEntry[]>([]);
  const [isConnected, setIsConnected] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const eventSourceRef = useRef<EventSource | null>(null);

  // Use refs to avoid reconnecting when callbacks change
  const onMessageRef = useRef(onMessage);
  const onErrorRef = useRef(onError);
  const levelFilterRef = useRef(levelFilter);

  useEffect(() => {
    onMessageRef.current = onMessage;
    onErrorRef.current = onError;
    levelFilterRef.current = levelFilter;
  }, [onMessage, onError, levelFilter]);

  const clearEntries = useCallback(() => {
    setEntries([]);
  }, []);

  useEffect(() => {
    if (!enabled) {
      // Cleanup existing connection
      if (eventSourceRef.current) {
        eventSourceRef.current.close();
        eventSourceRef.current = null;
        setIsConnected(false);
        setEntries([]);
      }
      return;
    }

    // Create SSE connection
    const eventSource = new EventSource("/api/logs/stream");
    eventSourceRef.current = eventSource;

    eventSource.onopen = () => {
      setIsConnected(true);
      setError(null);
    };

    eventSource.onmessage = (event) => {
      try {
        const entry = JSON.parse(event.data) as LogEntry;

        // Client-side level filtering using ref to avoid stale closure
        const currentLevelFilter = levelFilterRef.current;
        if (currentLevelFilter && entry.level !== currentLevelFilter) {
          return;
        }

        setEntries((prev) => [entry, ...prev]);
        onMessageRef.current?.(entry);
      } catch (err) {
        console.error("Failed to parse log entry:", err);
      }
    };

    eventSource.onerror = (err) => {
      console.error("SSE connection error:", err);
      setIsConnected(false);
      setError("Connection lost, reconnecting...");
      onErrorRef.current?.(err);
      // EventSource will auto-reconnect
    };

    // Cleanup on unmount
    return () => {
      eventSource.close();
      eventSourceRef.current = null;
      setIsConnected(false);
      setEntries([]);
    };
  }, [enabled]); // Only reconnect when enabled changes

  return {
    entries,
    isConnected,
    error,
    clearEntries,
  };
}
