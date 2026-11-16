-- skaka のスキーマ（適用済みのファイルは書き換えず、変更は新しい番号の V<n>__*.sql を追加する）

create table app_user (
  id            uuid primary key,
  sub           text not null unique,            -- OIDC の sub（開発用ログインは dev:名前）
  name          text not null,
  email         text,
  avatar_url    text,
  timezone      text not null default 'Asia/Tokyo',
  disabled      boolean not null default false,
  created_at    timestamptz not null default now(),
  last_login_at timestamptz
);
create index app_user_email_idx on app_user (lower(email));

create table session (
  token_hash bytea primary key,                  -- Cookie の値の SHA-256（値そのものは保存しない）
  user_id    uuid not null references app_user(id) on delete cascade,
  id_token   text,                               -- IdP からログアウトするときの id_token_hint
  created_at timestamptz not null default now(),
  expires_at timestamptz not null
);
create index session_expires_idx on session (expires_at);

-- OIDC のログイン途中の状態（複数ノードのどこに戻ってきてもよいよう DB に置く）
create table login_state (
  state      text primary key,
  verifier   text not null,
  nonce      text not null,
  return_to  text not null default '/',
  created_at timestamptz not null default now()
);

create table api_token (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  name         text not null,
  prefix       text not null,                 -- 画面に出す先頭部分（skaka_xxxxxx）
  token_hash   bytea not null unique,
  scope        text not null check (scope in ('read', 'write')),
  created_at   timestamptz not null default now(),
  expires_at   timestamptz,
  last_used_at timestamptz,
  revoked_at   timestamptz,
  oauth_client text                           -- 外部のサービス（skuaka）に OAuth で発行したもの（OAUTH_CLIENTS の id）
);
create index api_token_user_idx on api_token (user_id);

-- 外部のサービス（skuaka）への認可の認可コード（60秒・1回だけ. DB にはハッシュだけ）
create table oauth_code (
  code_hash      bytea primary key,
  client_id      text not null,
  redirect_uri   text not null,
  user_id        uuid not null references app_user(id) on delete cascade,
  code_challenge text not null,
  scope          text not null,
  expires_at     timestamptz not null,
  used_at        timestamptz
);

-- 会議室. URL は /r/{code}. personal は1人に1つの「自分の会議室」
create table room (
  id           uuid primary key,
  code         text not null unique,
  owner_id     uuid not null references app_user(id) on delete cascade,
  name         text not null,
  description  text not null default '',
  personal     boolean not null default false,
  guest_access boolean not null default false,    -- ログインしない人（ゲスト）も入れる（主催者の許可を待つ）
  lobby        boolean not null default false,    -- メンバーも主催者の許可を待つ
  -- 結び付いた skuaka の予定（作ったとき・予定から会議室を作ったときの写し）
  event_id      uuid,
  event_title   text,
  event_start   text,
  event_end     text,
  event_all_day boolean,
  created_at   timestamptz not null default now(),
  last_used_at timestamptz
);
create unique index room_personal_idx on room (owner_id) where personal;
create index room_owner_idx on room (owner_id);
create index room_event_idx on room (event_id) where event_id is not null;

-- 入ったことのある会議室（「最近の会議室」）
create table room_visit (
  user_id    uuid not null references app_user(id) on delete cascade,
  room_id    uuid not null references room(id) on delete cascade,
  visited_at timestamptz not null default now(),
  primary key (user_id, room_id)
);
create index room_visit_recent_idx on room_visit (user_id, visited_at desc);

-- 開いた会議（会議室に人が入ってから全員が出るまで）
create table meeting (
  id         uuid primary key,
  room_id    uuid not null references room(id) on delete cascade,
  started_at timestamptz not null default now(),
  ended_at   timestamptz
);
create index meeting_room_idx on meeting (room_id, started_at desc);

create table meeting_participant (
  id         uuid primary key,
  meeting_id uuid not null references meeting(id) on delete cascade,
  user_id    uuid references app_user(id) on delete set null,
  name       text not null,
  guest      boolean not null default false,
  joined_at  timestamptz not null default now(),
  left_at    timestamptz
);
create index meeting_participant_meeting_idx on meeting_participant (meeting_id);
create index meeting_participant_user_idx on meeting_participant (user_id, joined_at desc);

-- 全員宛のチャット（個別のメッセージは保存しない）
create table chat_message (
  id         uuid primary key,
  meeting_id uuid not null references meeting(id) on delete cascade,
  user_id    uuid references app_user(id) on delete set null,
  name       text not null,
  body       text not null,
  created_at timestamptz not null default now()
);
create index chat_message_meeting_idx on chat_message (meeting_id, created_at);

-- skuaka（カレンダー）との連携. skuaka の API トークンを AES-GCM で暗号化して保存する
create table skuaka_link (
  user_id    uuid primary key references app_user(id) on delete cascade,
  token_enc  bytea not null,
  prefix     text not null,
  account    text not null,                      -- skuaka での名前
  can_write  boolean not null,
  created_at timestamptz not null default now()
);

-- ---------------------------------------------------------------- skawau（ファイル共有）との連携と、会議の録画（保存先は skawau）

-- skawau の OAuth で受け取ったトークン（SECRET_KEY の鍵で AES-GCM で暗号化）
create table skawau_link (
  user_id    uuid primary key references app_user(id) on delete cascade,
  token_enc  bytea not null,
  created_at timestamptz not null default now()
);

-- skawau の許可の画面へ送り出してから戻るまでの状態（PKCE の verifier. 10分・1回だけ）
create table oauth_state (
  state      text primary key,
  user_id    uuid not null references app_user(id) on delete cascade,
  verifier   text not null,
  return_to  text not null,
  expires_at timestamptz not null
);

-- 録画. 録画中の中身はサーバーの一時ファイル（RECORDING_DIR）に追記し、終わったら skawau へ送って消す
create table recording (
  id          uuid primary key,
  room_id     uuid not null references room(id) on delete cascade,
  meeting_id  uuid references meeting(id) on delete set null,
  user_id     uuid not null references app_user(id) on delete cascade,  -- 録画した人（skawau のその人のところに置く）
  title       text not null,
  mime        text not null,
  share       boolean not null,                                        -- 参加者と共有する
  status      text not null,                                           -- recording / saving / saved / failed
  bytes       bigint not null default 0,
  duration_ms bigint,
  started_at  timestamptz not null,
  ended_at    timestamptz,
  file_id     uuid,                                                    -- skawau のファイル
  file_url    text,
  shared      int not null default 0,
  error       text
);
create index recording_meeting_idx on recording (meeting_id);
create index recording_room_idx on recording (room_id, started_at desc);
create index recording_open_idx on recording (status) where status in ('recording', 'saving');
