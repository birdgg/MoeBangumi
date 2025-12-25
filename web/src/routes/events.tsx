import { createFileRoute } from "@tanstack/react-router";
import { EventsPage } from "@/features/events/page";

export const Route = createFileRoute("/events")({
  component: EventsPage,
});
