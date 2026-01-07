use axum::{
    extract::{Query, State},
    Json,
};
use mikan::Season;
use serde::Deserialize;
#[cfg(feature = "openapi")]
use utoipa::IntoParams;

use crate::{error::AppResult, models::CalendarDay, services::CalendarService, state::AppState};

/// Query parameters for calendar endpoints
#[derive(Debug, Deserialize)]
#[cfg_attr(feature = "openapi", derive(IntoParams))]
pub struct CalendarQuery {
    /// Year (e.g., 2025). Defaults to current year.
    pub year: Option<i32>,
    /// Season: winter, spring, summer, fall. Defaults to current season.
    pub season: Option<Season>,
}

impl CalendarQuery {
    /// Get year and season, defaulting to current if not specified
    fn resolve(&self) -> (i32, Season) {
        let (current_year, current_season) = CalendarService::current_season();
        (
            self.year.unwrap_or(current_year),
            self.season.unwrap_or(current_season),
        )
    }
}

/// Get calendar (weekly anime schedule)
///
/// Returns cached data from database. If the database is empty for the requested season,
/// automatically fetches from Mikan/BGM.tv and populates the database.
/// Defaults to current season if year/season not specified.
#[cfg_attr(feature = "openapi", utoipa::path(
    get,
    path = "/api/calendar",
    tag = "calendar",
    params(CalendarQuery),
    responses(
        (status = 200, description = "Weekly anime schedule", body = Vec<CalendarDay>)
    )
))]
pub async fn get_calendar(
    State(state): State<AppState>,
    Query(query): Query<CalendarQuery>,
) -> AppResult<Json<Vec<CalendarDay>>> {
    let (year, season) = query.resolve();

    // Try to get from database first
    let calendar = state.calendar.get_calendar(year, season).await?;

    // If database is empty for this season, import seed data for this specific season
    if calendar.is_empty() {
        tracing::info!(
            "Calendar database is empty for {} {:?}, importing seed data for this season",
            year,
            season
        );
        state.calendar.import_season_if_missing(year, season).await?;
        let calendar = state.calendar.get_calendar(year, season).await?;
        return Ok(Json(calendar));
    }

    Ok(Json(calendar))
}

/// Refresh calendar data
///
/// Re-imports calendar data from GitHub seed file.
/// Returns the updated calendar data.
/// Defaults to current season if year/season not specified.
#[cfg_attr(feature = "openapi", utoipa::path(
    post,
    path = "/api/calendar/refresh",
    tag = "calendar",
    params(CalendarQuery),
    responses(
        (status = 200, description = "Calendar refreshed successfully", body = Vec<CalendarDay>)
    )
))]
pub async fn refresh_calendar(
    State(state): State<AppState>,
    Query(query): Query<CalendarQuery>,
) -> AppResult<Json<Vec<CalendarDay>>> {
    let (year, season) = query.resolve();
    tracing::info!("Manual calendar refresh requested for {} {:?}", year, season);
    state.calendar.import_seed_data().await?;
    let calendar = state.calendar.get_calendar(year, season).await?;
    Ok(Json(calendar))
}
