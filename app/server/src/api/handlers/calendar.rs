use axum::{
    extract::{Query, State},
    Json,
};
use mikan::Season;
use serde::Deserialize;

use crate::{error::AppResult, models::CalendarDay, services::CalendarService, state::AppState};

/// Query parameters for calendar endpoints
#[derive(Debug, Deserialize)]
pub struct CalendarQuery {
    pub year: Option<i32>,
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

pub async fn get_calendar(
    State(state): State<AppState>,
    Query(query): Query<CalendarQuery>,
) -> AppResult<Json<Vec<CalendarDay>>> {
    let (year, season) = query.resolve();

    // Try to get from database first
    let calendar = state.services.calendar.get_calendar(year, season).await?;

    // If database is empty for this season, import seed data for this specific season
    if calendar.is_empty() {
        tracing::info!(
            "Calendar database is empty for {} {:?}, importing seed data for this season",
            year,
            season
        );
        state
            .services
            .calendar
            .import_season_if_missing(year, season)
            .await?;
        let calendar = state.services.calendar.get_calendar(year, season).await?;
        return Ok(Json(calendar));
    }

    Ok(Json(calendar))
}

pub async fn refresh_calendar(
    State(state): State<AppState>,
    Query(query): Query<CalendarQuery>,
) -> AppResult<Json<Vec<CalendarDay>>> {
    let (year, season) = query.resolve();
    tracing::info!(
        "Manual calendar refresh requested for {} {:?}",
        year,
        season
    );
    state.services.calendar.import_season(year, season).await?;
    let calendar = state.services.calendar.get_calendar(year, season).await?;
    Ok(Json(calendar))
}
