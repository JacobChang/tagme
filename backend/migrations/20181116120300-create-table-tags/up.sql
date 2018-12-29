CREATE TABLE IF NOT EXISTS tagme.tags(
    id BIGSERIAL,
    user_id BIGINT NOT NULL,
    group_id BIGINT NOT NULL,
    title VARCHAR NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    updated_at TIMESTAMP WITH TIME ZONE,
    removed_at TIMESTAMP WITH TIME ZONE,
    status INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT tags_pkey PRIMARY KEY (id),
    CONSTRAINT tags_user_group_title_unique_key UNIQUE (user_id, group_id, title)
)