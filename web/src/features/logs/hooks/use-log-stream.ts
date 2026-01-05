import { useEffect, useCallback, useRef } from "react";
import { toast } from "sonner";

interface Log {
  id: number;
  created_at: string;
  level: "info" | "warning" | "error";
  message: string;
}

export function useLogStream() {
  // Track whether we've finished receiving initial historical logs
  // Only show toast for logs that arrive after initialization
  const isInitializedRef = useRef(false);

  const handleLog = useCallback((log: Log) => {
    // Skip toast for historical logs during initial connection
    if (!isInitializedRef.current) {
      return;
    }

    // Only show toast for warning and error levels
    if (log.level === "error") {
      toast.error(log.message, {
        duration: 8000,
      });
    } else if (log.level === "warning") {
      toast.warning(log.message, {
        duration: 6000,
      });
    }
  }, []);

  useEffect(() => {
    isInitializedRef.current = false;
    const eventSource = new EventSource("/api/logs/stream");

    // Use a small delay to allow initial batch of historical logs to be processed
    // before enabling toast notifications for new logs
    const initTimer = setTimeout(() => {
      isInitializedRef.current = true;
    }, 1000);

    eventSource.onmessage = (e) => {
      try {
        const log = JSON.parse(e.data) as Log;
        handleLog(log);
      } catch (err) {
        console.error("Failed to parse SSE log:", err);
      }
    };

    eventSource.onerror = () => {
      console.error("SSE connection error, will retry...");
      eventSource.close();
    };

    return () => {
      clearTimeout(initTimer);
      eventSource.close();
    };
  }, [handleLog]);
}
