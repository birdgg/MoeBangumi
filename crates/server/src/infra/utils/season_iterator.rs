use mikan::Season;

/// Iterator that traverses seasons backwards from current to a target season
pub struct SeasonIterator {
    current_year: i32,
    current_season: Season,
    end_year: i32,
    end_season: Season,
    finished: bool,
}

impl SeasonIterator {
    /// Create iterator from current season to target (inclusive)
    pub fn from_current_to(end_year: i32, end_season: Season) -> Self {
        let (current_year, current_season) = crate::services::CalendarService::current_season();
        Self {
            current_year,
            current_season,
            end_year,
            end_season,
            finished: false,
        }
    }

    /// Create iterator from specified start to target (inclusive)
    pub fn new(start_year: i32, start_season: Season, end_year: i32, end_season: Season) -> Self {
        Self {
            current_year: start_year,
            current_season: start_season,
            end_year,
            end_season,
            finished: false,
        }
    }

    /// Get previous season
    fn prev_season(year: i32, season: Season) -> (i32, Season) {
        match season {
            Season::Winter => (year - 1, Season::Fall),
            Season::Spring => (year, Season::Winter),
            Season::Summer => (year, Season::Spring),
            Season::Fall => (year, Season::Summer),
        }
    }

    /// Check if current position is at or after the end
    fn is_at_or_after_end(&self) -> bool {
        if self.current_year > self.end_year {
            true
        } else if self.current_year < self.end_year {
            false
        } else {
            // Same year, compare seasons
            Self::season_order(self.current_season) >= Self::season_order(self.end_season)
        }
    }

    fn season_order(season: Season) -> u8 {
        match season {
            Season::Winter => 0,
            Season::Spring => 1,
            Season::Summer => 2,
            Season::Fall => 3,
        }
    }
}

impl Iterator for SeasonIterator {
    type Item = (i32, Season);

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        if !self.is_at_or_after_end() {
            self.finished = true;
            return None;
        }

        let result = (self.current_year, self.current_season);

        // Check if we just returned the end season
        if self.current_year == self.end_year && self.current_season == self.end_season {
            self.finished = true;
        } else {
            // Move to previous season
            let (prev_year, prev_season) = Self::prev_season(self.current_year, self.current_season);
            self.current_year = prev_year;
            self.current_season = prev_season;
        }

        Some(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_season_iterator() {
        let iter = SeasonIterator::new(2024, Season::Fall, 2024, Season::Spring);
        let seasons: Vec<_> = iter.collect();

        assert_eq!(seasons.len(), 3);
        assert_eq!(seasons[0], (2024, Season::Fall));
        assert_eq!(seasons[1], (2024, Season::Summer));
        assert_eq!(seasons[2], (2024, Season::Spring));
    }

    #[test]
    fn test_cross_year() {
        let iter = SeasonIterator::new(2024, Season::Spring, 2023, Season::Fall);
        let seasons: Vec<_> = iter.collect();

        assert_eq!(seasons.len(), 3);
        assert_eq!(seasons[0], (2024, Season::Spring));
        assert_eq!(seasons[1], (2024, Season::Winter));
        assert_eq!(seasons[2], (2023, Season::Fall));
    }
}
