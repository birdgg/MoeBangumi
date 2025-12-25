import { useEffect, useCallback } from "react";
import { toast } from "sonner";

interface Event {
  id: number;
  created_at: string;
  level: "info" | "warning" | "error";
  message: string;
  details?: string;
}

export function useEventStream() {
  const handleEvent = useCallback((event: Event) => {
    // Only show toast for warning and error levels
    if (event.level === "error") {
      toast.error(event.message, {
        duration: 8000,
      });
    } else if (event.level === "warning") {
      toast.warning(event.message, {
        duration: 6000,
      });
    }
  }, []);

  useEffect(() => {
    const eventSource = new EventSource("/api/events/stream");

    eventSource.onmessage = (e) => {
      try {
        const event = JSON.parse(e.data) as Event;
        handleEvent(event);
      } catch (err) {
        console.error("Failed to parse SSE event:", err);
      }
    };

    eventSource.onerror = () => {
      console.error("SSE connection error, will retry...");
      eventSource.close();
    };

    return () => {
      eventSource.close();
    };
  }, [handleEvent]);
}
