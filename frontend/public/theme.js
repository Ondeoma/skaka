// 画面の色（ライト・ダーク）を、描く前に決める（ちらつかないように）. CSP でインラインのスクリプトを禁じているため、別のファイルにしている
try { var t = localStorage.getItem('skaka.theme'); if (t === 'light' || t === 'dark') document.documentElement.setAttribute('data-theme', t) } catch (e) {}
