-- ---------------------------------------------------------------- 文字起こし（Whisper）と議事録（skakapo）

-- 文字起こしの1行（話した人の端末が送った音声を、サーバーが Whisper で文字にしたもの）
create table transcript_line (
  id         uuid primary key,
  meeting_id uuid not null references meeting(id) on delete cascade,
  user_id    uuid references app_user(id) on delete set null,
  name       text not null,
  body       text not null,
  spoken_at  timestamptz not null                 -- 話し始めた時刻
);
create index transcript_line_meeting_idx on transcript_line (meeting_id, spoken_at);

-- skakapo（AI エージェント）との連携. skakapo の API トークンを AES-GCM で暗号化して保存する（skuaka_link と同じ形）
create table skakapo_link (
  user_id    uuid primary key references app_user(id) on delete cascade,
  token_enc  bytea not null,
  prefix     text not null,
  account    text not null,                      -- skakapo での名前
  can_write  boolean not null,
  created_at timestamptz not null default now()
);

-- 議事録・要約. skakapo に会話（依頼）を作って文字起こしを渡し、エージェントの返事を取り込む. 取り込んだら頼んだ人の skawau に置く
create table minutes (
  id          uuid primary key,
  meeting_id  uuid not null references meeting(id) on delete cascade,
  user_id     uuid not null references app_user(id) on delete cascade,  -- 頼んだ人（その人の skakapo の会話）
  session_id  uuid not null,                                           -- skakapo の会話
  session_url text not null,
  title       text not null,                                           -- 会議室の名前と日時（skawau に置くファイルの名前にも使う）
  status      text not null,                                           -- running / done / failed
  body        text not null default '',
  error       text,
  file_id     uuid,                                                    -- skawau に置いたファイル（取り込み直すと中身を書き換える）
  file_url    text,
  file_error  text,                                                    -- skawau に置けなかった理由
  created_at  timestamptz not null default now(),
  updated_at  timestamptz not null default now()
);
create index minutes_meeting_idx on minutes (meeting_id, created_at desc);
create index minutes_running_idx on minutes (status) where status = 'running';
