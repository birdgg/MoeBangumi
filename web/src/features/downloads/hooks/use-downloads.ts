// Downloads feature - disabled until backend API is implemented
// The following APIs are not yet available: listTorrents, deleteTorrents

// Local type definitions for Task (backend doesn't export these yet)
export type TaskStatus =
  | "downloading"
  | "seeding"
  | "completed"
  | "paused"
  | "checking"
  | "queued"
  | "stalled"
  | "error"
  | "unknown";

export interface Task {
  id: string;
  name: string;
  status: TaskStatus;
  progress: number;
  total_size: number;
}

// Stub hook - returns empty data
export function useListTorrents() {
  return {
    data: [] as Task[],
    isLoading: false,
    error: null as Error | null,
    refetch: () => {},
  };
}

// Stub hook - no-op mutation
export function useDeleteTorrents() {
  return {
    mutate: (_params: { body: { hashes: string[]; delete_files: boolean } }) => {},
    isPending: false,
  };
}
