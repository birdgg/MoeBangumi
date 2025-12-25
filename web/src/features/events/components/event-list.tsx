import { EventItem } from "./event-item";

interface Event {
  id: number;
  created_at: string;
  level: "info" | "warning" | "error";
  message: string;
  details?: string;
}

interface EventListProps {
  events: Event[];
}

export function EventList({ events }: EventListProps) {
  if (events.length === 0) {
    return (
      <div className="py-12 text-center text-muted-foreground font-mono text-sm">
        暂无事件记录
      </div>
    );
  }

  return (
    <div className="divide-y divide-muted/30">
      {events.map((event) => (
        <EventItem key={event.id} event={event} />
      ))}
    </div>
  );
}
