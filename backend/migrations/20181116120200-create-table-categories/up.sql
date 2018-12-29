CREATE TABLE IF NOT EXISTS tagme.categories(
    id BIGSERIAL,
    user_id BIGINT NOT NULL,
    title VARCHAR NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    updated_at TIMESTAMP WITH TIME ZONE,
    removed_at TIMESTAMP WITH TIME ZONE,
    status INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT categories_pkey PRIMARY KEY (id),
    CONSTRAINT categories_user_title_uninue_key UNIQUE (user_id, title)
)