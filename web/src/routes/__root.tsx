import { createRootRoute, Outlet } from "@tanstack/react-router";
import { AppLayout } from "@/components/app-layout";
import { UpdateBanner } from "@/components/update-banner";

export const Route = createRootRoute({
  component: RootLayout,
});

function RootLayout() {
  return (
    <AppLayout>
      <UpdateBanner />
      <Outlet />
    </AppLayout>
  );
}
