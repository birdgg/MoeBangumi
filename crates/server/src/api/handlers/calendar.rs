use axum::{extract::State, Json};
use bgmtv::CalendarDay;

use crate::{error::AppResult, state::AppState};

/// Get BGM.tv calendar (weekly anime schedule)
///
/// Returns cached data from database. If the database is empty,
/// automatically fetches from BGM.tv API and populates the database.
#[utoipa::path(
    get,
    path = "/api/calendar",
    tag = "calendar",
    responses(
        (status = 200, description = "Weekly anime schedule", body = Vec<CalendarDay>)
    )
)]
pub async fn get_calendar(State(state): State<AppState>) -> AppResult<Json<Vec<CalendarDay>>> {
    // Try to get from database first
    let calendar = state.calendar.get_calendar().await?;

    // If database is empty, fetch from API and cache it
    if calendar.is_empty() {
        tracing::info!("Calendar database is empty, fetching from BGM.tv API");
        state.calendar.refresh_calendar().await?;
        let calendar = state.calendar.get_calendar().await?;
        return Ok(Json(calendar));
    }

    Ok(Json(calendar))
}

/// Refresh BGM.tv calendar data
///
/// Forces a refresh of the calendar data from BGM.tv API.
/// Returns the updated calendar data.
#[utoipa::path(
    post,
    path = "/api/calendar/refresh",
    tag = "calendar",
    responses(
        (status = 200, description = "Calendar refreshed successfully", body = Vec<CalendarDay>)
    )
)]
pub async fn refresh_calendar(State(state): State<AppState>) -> AppResult<Json<Vec<CalendarDay>>> {
    tracing::info!("Manual calendar refresh requested");
    state.calendar.refresh_calendar().await?;
    let calendar = state.calendar.get_calendar().await?;
    Ok(Json(calendar))
}
