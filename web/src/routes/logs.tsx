import { createFileRoute } from "@tanstack/react-router";
import { LogsPage } from "@/features/logs/page";

export const Route = createFileRoute("/logs")({
  component: LogsPage,
});
