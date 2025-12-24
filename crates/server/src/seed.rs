use sqlx::SqlitePool;

use crate::models::CreateBangumi;
use crate::repositories::BangumiRepository;

/// Seed the database with sample bangumi data for development
pub async fn seed_bangumi(pool: &SqlitePool) -> Result<(), sqlx::Error> {
    // Check if we already have data
    let existing = BangumiRepository::get_all(pool).await?;
    if !existing.is_empty() {
        tracing::info!(
            "Database already has {} bangumi, skipping seed",
            existing.len()
        );
        return Ok(());
    }

    tracing::info!("Seeding database with sample bangumi...");

    let seed_data = vec![
        CreateBangumi {
            title_chinese: "葬送的芙莉莲".to_string(),
            title_japanese: Some("葬送のフリーレン".to_string()),
            title_original_chinese: "葬送的芙莉莲".to_string(),
            title_original_japanese: Some("葬送のフリーレン".to_string()),
            season: 1,
            year: 2023,
            bgmtv_id: Some(400602),
            tmdb_id: Some(209867),
            poster_url: Some(
                "https://lain.bgm.tv/r/400/pic/cover/l/c5/88/395378_jztpO.jpg".to_string(),
            ),
            air_date: Some("2023-09-29".to_string()),
            air_week: Some(5),
            total_episodes: 28,
            episode_offset: 0,
            auto_download: true,
            save_path: None,
            source_type: crate::models::SourceType::WebRip,
            finished: true,
            kind: Some("TV".to_string()),
            rss_entries: vec![],
        },
        CreateBangumi {
            title_chinese: "迷宫饭".to_string(),
            title_japanese: Some("ダンジョン飯".to_string()),
            title_original_chinese: "迷宫饭".to_string(),
            title_original_japanese: Some("Dungeon Meshi".to_string()),
            season: 1,
            year: 2024,
            bgmtv_id: Some(425977),
            tmdb_id: Some(119121),
            poster_url: None,
            air_date: Some("2024-01-04".to_string()),
            air_week: Some(4),
            total_episodes: 24,
            episode_offset: 0,
            auto_download: true,
            save_path: None,
            source_type: crate::models::SourceType::WebRip,
            finished: false,
            kind: Some("TV".to_string()),
            rss_entries: vec![],
        },
        CreateBangumi {
            title_chinese: "药屋少女的呢喃".to_string(),
            title_japanese: Some("薬屋のひとりごと".to_string()),
            title_original_chinese: "药屋少女的呢喃".to_string(),
            title_original_japanese: Some("Kusuriya no Hitorigoto".to_string()),
            season: 1,
            year: 2023,
            bgmtv_id: Some(413084),
            tmdb_id: Some(114226),
            poster_url: None,
            air_date: Some("2023-10-21".to_string()),
            air_week: Some(6),
            total_episodes: 24,
            episode_offset: 0,
            auto_download: true,
            save_path: None,
            source_type: crate::models::SourceType::WebRip,
            finished: true,
            kind: Some("TV".to_string()),
            rss_entries: vec![],
        },
    ];

    for data in seed_data {
        let title = data.title_chinese.clone();
        match BangumiRepository::create(pool, data).await {
            Ok(_) => tracing::info!("Created seed bangumi: {}", title),
            Err(e) => tracing::warn!("Failed to create seed bangumi {}: {}", title, e),
        }
    }

    tracing::info!("Seed data created successfully");
    Ok(())
}
