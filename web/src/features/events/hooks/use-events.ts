import { useInfiniteQuery } from "@tanstack/react-query";

interface Event {
  id: number;
  created_at: string;
  level: "info" | "warning" | "error";
  message: string;
  details?: string;
}

interface UseEventsParams {
  level?: string;
  pageSize?: number;
}

const PAGE_SIZE = 50;

async function fetchEvents(params: {
  level?: string;
  limit: number;
  offset: number;
}): Promise<Event[]> {
  const searchParams = new URLSearchParams();
  if (params.level) searchParams.set("level", params.level);
  searchParams.set("limit", params.limit.toString());
  searchParams.set("offset", params.offset.toString());

  const response = await fetch(`/api/events?${searchParams.toString()}`);

  if (!response.ok) {
    throw new Error("Failed to fetch events");
  }

  return response.json();
}

export function useEvents(params: UseEventsParams = {}) {
  const pageSize = params.pageSize ?? PAGE_SIZE;

  return useInfiniteQuery({
    queryKey: ["events", params.level],
    queryFn: ({ pageParam = 0 }) =>
      fetchEvents({
        level: params.level,
        limit: pageSize,
        offset: pageParam,
      }),
    initialPageParam: 0,
    getNextPageParam: (lastPage, allPages) => {
      // If we got fewer items than requested, we've reached the end
      if (lastPage.length < pageSize) {
        return undefined;
      }
      // Return the next offset
      return allPages.length * pageSize;
    },
    refetchInterval: 30000, // Auto-refresh every 30 seconds
  });
}
