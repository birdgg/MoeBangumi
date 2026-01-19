import { useLogStream } from "@/features/logs/hooks/use-log-stream";

interface LogStreamProviderProps {
  children: React.ReactNode;
}

export function LogStreamProvider({ children }: LogStreamProviderProps) {
  useLogStream();
  return <>{children}</>;
}
