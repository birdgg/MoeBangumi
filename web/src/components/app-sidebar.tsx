import {
  Sidebar,
  SidebarContent,
  SidebarGroup,
  SidebarHeader,
  SidebarMenu,
  SidebarMenuButton,
  SidebarMenuItem,
} from "@/components/ui/sidebar";
import {
  IconDeviceTv,
  IconCalendarWeek,
  IconSettings,
  IconSparkles,
} from "@tabler/icons-react";
import { useLocation, useNavigate } from "@tanstack/react-router";

interface SidebarItem {
  id: string;
  label: string;
  icon: React.ReactNode;
  path?: "/" | "/settings";
}

const sidebarItems: SidebarItem[] = [
  { id: "anime", label: "动漫", icon: <IconDeviceTv />, path: "/" },
  { id: "schedule", label: "每日放送", icon: <IconCalendarWeek /> },
  { id: "settings", label: "设置", icon: <IconSettings />, path: "/settings" },
];

function MoeLogo() {
  return (
    <div className="group flex items-center gap-4">
      <div className="relative">
        <div className="absolute -inset-1 rounded-2xl bg-linear-to-br from-chart-1 via-chart-2 to-chart-3 opacity-75 blur-sm transition-all duration-300 group-hover:opacity-100 group-hover:blur-md" />
        <div className="relative flex size-10 items-center justify-center rounded-2xl bg-linear-to-br from-chart-1 via-chart-2 to-chart-3 text-base font-bold text-white shadow-lg transition-transform duration-300 group-hover:scale-110">
          <span className="drop-shadow-sm">M</span>
          <IconSparkles className="absolute -top-1 -right-1 size-3 text-white opacity-0 transition-all duration-300 group-hover:opacity-100 group-hover:rotate-12" />
        </div>
      </div>
      <span className="bg-linear-to-r from-chart-1 via-chart-2 to-chart-3 bg-clip-text text-lg font-bold tracking-tight text-transparent">
        MoeBangumi
      </span>
    </div>
  );
}

export function AppSidebar() {
  const location = useLocation();
  const navigate = useNavigate();

  const getActiveItem = () => {
    if (location.pathname === "/settings") return "settings";
    if (location.pathname === "/schedule") return "schedule";
    return "anime";
  };
  const activeItem = getActiveItem();

  return (
    <Sidebar collapsible="icon" className="border-r-0">
      <div className="pointer-events-none absolute inset-0 bg-linear-to-b from-chart-1/5 via-transparent to-chart-3/5" />

      <SidebarHeader className="relative z-10 p-4">
        <MoeLogo />
      </SidebarHeader>

      <SidebarContent className="relative z-10 pt-4">
        <SidebarGroup className="py-4">
          <SidebarMenu className="gap-4">
            {sidebarItems.map((item) => (
              <SidebarMenuItem key={item.id}>
                <SidebarMenuButton
                  onClick={item.path ? () => navigate({ to: item.path! }) : undefined}
                  className="rounded-xl text-base transition-all duration-300 hover:bg-chart-1/10 data-active:bg-chart-1/15 data-active:text-chart-1 [&_svg]:size-5 data-active:[&_svg]:text-chart-1"
                  isActive={activeItem === item.id}
                  tooltip={item.label}
                >
                  {item.icon}
                  <span>{item.label}</span>
                </SidebarMenuButton>
              </SidebarMenuItem>
            ))}
          </SidebarMenu>
        </SidebarGroup>
      </SidebarContent>
    </Sidebar>
  );
}
