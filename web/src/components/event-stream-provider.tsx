import { useEventStream } from "@/features/events/hooks/use-event-stream";

interface EventStreamProviderProps {
  children: React.ReactNode;
}

export function EventStreamProvider({ children }: EventStreamProviderProps) {
  useEventStream();
  return <>{children}</>;
}
