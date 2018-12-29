CREATE TABLE IF NOT EXISTS tagme.users(
    id BIGSERIAL,
    uuid uuid NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    updated_at TIMESTAMP WITH TIME ZONE,
    removed_at TIMESTAMP WITH TIME ZONE,
    status INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT users_pkey PRIMARY KEY (id),
    CONSTRAINT users_uuid_uninue_key UNIQUE (uuid)
)