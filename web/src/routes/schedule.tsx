import { createFileRoute } from "@tanstack/react-router";
import { SchedulePage } from "@/features/calendar/page";

export const Route = createFileRoute("/schedule")({
  component: SchedulePage,
});
