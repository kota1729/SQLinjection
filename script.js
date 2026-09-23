(function(){

/* ---------- ミニSQL評価エンジン ---------- */
function stripComments(s){
  let out=''; let inS=false, inD=false;
  for (let i=0;i<s.length;i++){
    const c=s[i];
    if (!inD && c==="'"){ inS=!inS; out+=c; continue; }
    if (!inS && c==='"'){ inD=!inD; out+=c; continue; }
    if (!inS && !inD){
      if (c==='-' && s[i+1]==='-') return out;
      if (c==='#') return out;
      if (c==='/' && s[i+1]==='*'){
        const end=s.indexOf('*/', i+2);
        if (end===-1) return out;
        i=end+1; continue;
      }
    }
    out+=c;
  }
  return out;
}
function tokenize(s){
  const tokens=[]; let i=0;
  while(i<s.length){
    const c=s[i];
    if (/\s/.test(c)){ i++; continue; }
    if (c==="'"||c==='"'){
      const q=c; let j=i+1, val='';
      while(j<s.length){
        if (s[j]===q){ if (s[j+1]===q){ val+=q; j+=2; continue; } j++; break; }
        val+=s[j]; j++;
      }
      tokens.push({type:'string', value:val}); i=j; continue;
    }
    if (/[0-9]/.test(c)){
      let j=i, val='';
      while(j<s.length && /[0-9.]/.test(s[j])){ val+=s[j]; j++; }
      tokens.push({type:'number', value:parseFloat(val)}); i=j; continue;
    }
    if (/[A-Za-z_]/.test(c)){
      let j=i, val='';
      while(j<s.length && /[A-Za-z0-9_]/.test(s[j])){ val+=s[j]; j++; }
      const up=val.toUpperCase();
      if (up==='AND') tokens.push({type:'and'});
      else if (up==='OR') tokens.push({type:'or'});
      else if (up==='NOT') tokens.push({type:'not'});
      else if (up==='LENGTH') tokens.push({type:'func', value:'LENGTH'});
      else tokens.push({type:'ident', value:val});
      i=j; continue;
    }
    if (c==='('){ tokens.push({type:'lparen'}); i++; continue; }
    if (c===')'){ tokens.push({type:'rparen'}); i++; continue; }
    if (c==='|' && s[i+1]==='|'){ tokens.push({type:'or'}); i+=2; continue; }
    if (c==='!' && s[i+1]==='='){ tokens.push({type:'op', value:'!='}); i+=2; continue; }
    if (c==='<' && s[i+1]==='>'){ tokens.push({type:'op', value:'!='}); i+=2; continue; }
    if (c==='<' && s[i+1]==='='){ tokens.push({type:'op', value:'<='}); i+=2; continue; }
    if (c==='>' && s[i+1]==='='){ tokens.push({type:'op', value:'>='}); i+=2; continue; }
    if (c==='='){ tokens.push({type:'op', value:'='}); i++; continue; }
    if (c==='<'){ tokens.push({type:'op', value:'<'}); i++; continue; }
    if (c==='>'){ tokens.push({type:'op', value:'>'}); i++; continue; }
    i++;
  }
  return tokens;
}
function parseTokens(tokens){
  let pos=0;
  const peek=()=>tokens[pos];
  const next=()=>tokens[pos++];
  function pExpr(){ return pOr(); }
  function pOr(){ let n=pAnd(); while(peek()&&peek().type==='or'){ next(); n={type:'or', left:n, right:pAnd()}; } return n; }
  function pAnd(){ let n=pNot(); while(peek()&&peek().type==='and'){ next(); n={type:'and', left:n, right:pNot()}; } return n; }
  function pNot(){ if (peek()&&peek().type==='not'){ next(); return {type:'not', node:pNot()}; } return pPrimary(); }
  function pPrimary(){
    if (peek()&&peek().type==='lparen'){ next(); const n=pExpr(); if (peek()&&peek().type==='rparen') next(); return n; }
    return pCmp();
  }
  function pOperand(){
    const t=peek();
    if (!t) return {type:'lit', value:null};
    if (t.type==='func'){
      next(); let arg=null;
      if (peek()&&peek().type==='lparen'){ next(); arg=pOperand(); if (peek()&&peek().type==='rparen') next(); }
      return {type:'func', name:t.value, arg};
    }
    if (t.type==='string'){ next(); return {type:'lit', value:t.value}; }
    if (t.type==='number'){ next(); return {type:'lit', value:t.value}; }
    if (t.type==='ident'){ next(); return {type:'col', name:t.value}; }
    next(); return {type:'lit', value:null};
  }
  function pCmp(){
    const left=pOperand();
    const t=peek();
    if (t && t.type==='op'){ next(); const right=pOperand(); return {type:'cmp', op:t.value, left, right}; }
    return {type:'truthy', node:left};
  }
  return pExpr();
}
function resolveOperand(node,row){
  if (!node) return null;
  if (node.type==='lit') return node.value;
  if (node.type==='col'){
    const key=Object.keys(row).find(k=>k.toLowerCase()===node.name.toLowerCase());
    return key? row[key] : null;
  }
  if (node.type==='func'){
    const v=resolveOperand(node.arg,row);
    if (node.name==='LENGTH') return v==null? 0 : String(v).length;
  }
  return null;
}
function evalAst(ast,row){
  switch(ast.type){
    case 'or': return evalAst(ast.left,row) || evalAst(ast.right,row);
    case 'and': return evalAst(ast.left,row) && evalAst(ast.right,row);
    case 'not': return !evalAst(ast.node,row);
    case 'cmp': {
      let a=resolveOperand(ast.left,row), b=resolveOperand(ast.right,row);
      if (typeof a==='number' || typeof b==='number'){ a=Number(a); b=Number(b); }
      else { a=String(a); b=String(b); }
      switch(ast.op){
        case '=': return a===b;
        case '!=': return a!==b;
        case '<': return a<b;
        case '>': return a>b;
        case '<=': return a<=b;
        case '>=': return a>=b;
      }
      return false;
    }
    case 'truthy': {
      const v=resolveOperand(ast.node,row);
      return typeof v==='number' ? v!==0 : !!v;
    }
  }
  return false;
}
function evaluateClause(rawClause,row){
  try{
    const cleaned=stripComments(rawClause);
    const tokens=tokenize(cleaned);
    if (tokens.length===0) return false;
    return !!evalAst(parseTokens(tokens),row);
  }catch(e){ return false; }
}
function firstMatch(clause,table){
  for (const row of table){ if (evaluateClause(clause,row)) return row; }
  return null;
}
function esc(s){ return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;'); }

/* ---------- 乱数(シード付き) ---------- */
function seededRng(seedStr){
  let h = 1779033703 ^ seedStr.length;
  for (let i=0;i<seedStr.length;i++){
    h = Math.imul(h ^ seedStr.charCodeAt(i), 3432918353);
    h = (h << 13) | (h >>> 19);
  }
  let a = (h >>> 0) || 1;
  return function(){
    a += 0x6D2B79F5;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
function pick(rng, arr){ return arr[Math.floor(rng()*arr.length)]; }
function randInt(rng, min, max){ return min + Math.floor(rng()*(max-min+1)); }
function randHex(rng, len){
  const base='0123456789abcdef'; let s='';
  for (let i=0;i<len;i++) s += base[Math.floor(rng()*16)];
  return s;
}
function shuffled(rng, arr){
  const a = arr.slice();
  for (let i=a.length-1;i>0;i--){ const j=Math.floor(rng()*(i+1)); [a[i],a[j]]=[a[j],a[i]]; }
  return a;
}
function randPass(rng){
  const syll=['ka','ri','zu','mo','ta','ne','fu','sa','ryo','ke'];
  let s=''; for (let i=0;i<3;i++) s+=pick(rng,syll);
  return s.charAt(0).toUpperCase()+s.slice(1)+randInt(rng,10,99)+'!';
}

/* ---------- 問題生成(10系統) ---------- */
const TABLE_POOL = [
  {table:'users', jp:'ユーザー'}, {table:'members', jp:'会員'},
  {table:'accounts', jp:'アカウント'}, {table:'customers', jp:'顧客'},
];
const PRODUCT_POOL = [
  {table:'products', jp:'商品'}, {table:'items', jp:'アイテム'},
  {table:'inventory', jp:'在庫'}, {table:'listings', jp:'出品情報'},
];
const DOC_POOL = [
  {table:'articles', jp:'記事'}, {table:'posts', jp:'投稿'},
  {table:'documents', jp:'資料'}, {table:'memos', jp:'社内メモ'},
];
const SECRET_TABLE_POOL = ['admin_secrets','confidential','internal_vault','staff_notes'];
const USERNAME_COLS = ['username','login_id','account_name'];
const PASSWORD_COLS = ['password','passwd','secret_key'];
const GUARD_COLS = [{col:'active', trueVal:1}, {col:'enabled', trueVal:1}, {col:'is_locked', trueVal:0}];
const NORMAL_USERS = ['guest','staff01','tanaka','bob','yuki'];

function genA_orBypass(rng){
  const t = pick(rng, TABLE_POOL);
  const uCol = pick(rng, USERNAME_COLS);
  const others = shuffled(rng, NORMAL_USERS).slice(0,2);
  const rows = [
    { [uCol]:'admin' },
    { [uCol]:others[0] },
    { [uCol]:others[1] },
  ];
  const flag = `FLAG{or_bypass_${randHex(rng,4)}}`;
  return {
    title:`認証バイパス`, tag:'beginner',
    story:[
      `${t.jp}テーブル(<code>${t.table}</code>)の「ユーザー名検索」機能は、入力値をそのままクエリに埋め込んでいます。`,
      '本来は存在するユーザー名を入れないと何も表示されないはずですが、任意の入力で何らかのユーザーを表示させてください。'
    ],
    queryTpl:(u)=>`SELECT * FROM ${t.table} WHERE ${uCol} = '${esc(u)}';`,
    fields:[{key:'u', label:'ユーザー名'}],
    check(vals){
      const row = firstMatch(`${uCol} = '${vals.u}'`, rows);
      if (row) return { ok:true, msg:`検索結果に表示されました: ${row[uCol]}`, flag };
      return { ok:false, msg:'検索結果: 該当するユーザーはいません。' };
    },
    hint:'文字列を早めに閉じて、常に真になる条件を OR で追加すると何が起きるでしょうか。',
    explain:`入力値がそのままクエリに連結されるため、<code>' OR '1'='1</code> のような文字列を入れると WHERE句全体が常に真になり、テーブルの先頭行が常にマッチするようになります。`
  };
}

function genB_commentBypass(rng){
  const t = pick(rng, TABLE_POOL);
  const uCol = pick(rng, USERNAME_COLS), pCol = pick(rng, PASSWORD_COLS);
  const guard = pick(rng, GUARD_COLS);
  const others = shuffled(rng, NORMAL_USERS).slice(0,2);
  const rows = [
    { [uCol]:'admin', [pCol]:randPass(rng), [guard.col]: guard.trueVal===1?0:1 },
    { [uCol]:others[0], [pCol]:randPass(rng), [guard.col]: guard.trueVal },
    { [uCol]:others[1], [pCol]:randPass(rng), [guard.col]: guard.trueVal },
  ];
  const flag = `FLAG{comment_bypass_${randHex(rng,4)}}`;
  return {
    title:'コメントによる無効化', tag:'beginner',
    story:[
      `admin アカウントは現在 <code>${guard.col} = ${guard.trueVal===1?0:1}</code> の状態(無効・停止中)です。`,
      `クエリには <code>AND ${guard.col} = ${guard.trueVal}</code> の条件が追加されました。`,
      '目標: それでも admin としてログインしてください。'
    ],
    queryTpl:(u,p)=>`SELECT * FROM ${t.table} WHERE ${uCol} = '${esc(u)}' AND ${pCol} = '${esc(p)}' AND ${guard.col} = ${guard.trueVal};`,
    fields:[{key:'u', label:'ユーザー名'}, {key:'p', label:'パスワード'}],
    check(vals){
      const row = firstMatch(`${uCol} = '${vals.u}' AND ${pCol} = '${vals.p}' AND ${guard.col} = ${guard.trueVal}`, rows);
      if (row && row[uCol]==='admin') return { ok:true, msg:'ログイン成功: admin としてサインインしました(本来は無効化されているはずのアカウントで)。', flag };
      if (row) return { ok:false, msg:`${row[uCol]} としてログインできましたが、admin ではありません。` };
      return { ok:false, msg:'ログイン失敗: 条件が一致しません。' };
    },
    hint:'-- や # は、それより後ろのSQLをすべて無視させることができます。password と条件チェック自体を消してしまいましょう。',
    explain:`<code>admin' --</code> と入力すると、クエリは username = 'admin' だけが残り、password と追加条件のチェックが丸ごとコメントアウトされます。`
  };
}

function genC_numeric(rng){
  const t = pick(rng, PRODUCT_POOL);
  const hiddenId = randInt(rng, 100, 999);
  const flag = `FLAG{numeric_injection_${randHex(rng,4)}}`;
  const rows = [
    { id:1, name:'公開アイテムA' }, { id:2, name:'公開アイテムB' }, { id:3, name:'公開アイテムC' },
    { id:hiddenId, name:`非公開: 次世代${t.jp}試作`, secretFlag:flag },
  ];
  return {
    title:'数値型インジェクション', tag:'beginner',
    story:[
      `${t.jp}検索フォーム(<code>${t.table}</code>)です。ID はクオートで囲まれずそのまま埋め込まれます。`,
      'ID を知らなくても、非公開の情報を検索結果に含めてください。'
    ],
    queryTpl:(id)=>`SELECT * FROM ${t.table} WHERE id = ${esc(id)};`,
    fields:[{key:'id', label:'商品ID'}],
    check(vals){
      const target = rows[3];
      const hit = evaluateClause(`id = ${vals.id}`, target);
      if (hit) return { ok:true, msg:`非公開情報が検索結果に表示されました: 「${target.name}」`, flag };
      return { ok:false, msg:'検索結果: 該当する情報はありませんでした。' };
    },
    hint:'数値項目はクオートで囲まれていません。文字列を閉じる必要はなく、そのまま OR 条件を続けられます。',
    explain:`<code>0 OR 1=1</code> のように入力すると WHERE句がすべての行で真になり、ID未指定では出てこない非公開データまで結果に含まれます。`
  };
}

function genD_doubleQuote(rng){
  const t = pick(rng, DOC_POOL);
  const flagCol = pick(rng, ['private','restricted','internal_only']);
  const flag = `FLAG{double_quote_bypass_${randHex(rng,4)}}`;
  const rows = [
    { title:`公開${t.jp}: 今週のお知らせ`, [flagCol]:0 },
    { title:`社内限定: 未発表情報について`, [flagCol]:1, secretFlag:flag },
  ];
  return {
    title:'ダブルクオート編', tag:'intermediate',
    story:[
      `この${t.jp}検索フォーム(<code>${t.table}</code>)は文字列をダブルクオートで囲んでいます。`,
      '社内限定のタイトルを検索結果に表示させてください。'
    ],
    queryTpl:(q)=>`SELECT * FROM ${t.table} WHERE title = "${esc(q)}";`,
    fields:[{key:'q', label:'タイトル検索'}],
    check(vals){
      const target = rows[1];
      const hit = evaluateClause(`title = "${vals.q}"`, target);
      if (hit) return { ok:true, msg:'非公開の情報が検索結果に含まれました。', flag };
      return { ok:false, msg:'該当する情報はありませんでした。' };
    },
    hint:'シングルクオートで閉じようとしても失敗するはずです。今回は " を使いましょう。',
    explain:`<code>" OR "1"="1</code> のようにダブルクオートで文字列を閉じることで、常に真になる条件を追加できます。シングルクオート対策だけでは防げない典型例です。`
  };
}

function genE_orderBy(rng){
  const t = pick(rng, DOC_POOL);
  const cols = randInt(rng, 3, 8);
  const flag = `FLAG{order_by_enum_${randHex(rng,4)}}`;
  return {
    title:'ORDER BY でカラム数を調べる', tag:'intermediate',
    story:[
      `${t.jp}一覧(<code>${t.table}</code>)の並び替え機能です。<code>ORDER BY</code> の後ろに指定した列番号がそのまま使われます。`,
      'このテーブルの実際のカラム数を突き止めてください(次のレベルで使うことがあります)。'
    ],
    queryTpl:(n)=>`SELECT * FROM ${t.table} ORDER BY ${esc(n)};`,
    fields:[{key:'n', label:'並び替え列番号'}],
    check(vals){
      const n = parseInt(String(vals.n).trim(), 10);
      if (isNaN(n)) return { ok:false, msg:'数値を入力してください。' };
      if (n > cols) return { ok:false, msg:`データベースエラー: ORDER BY の列位置 ${n} は範囲外です(Unknown column '${n}' in 'order clause')。` };
      if (n < cols) return { ok:false, msg:'並び替えは正常に実行されました。もっと大きい数字も試して、エラーになる境目を探してみましょう。' };
      return { ok:true, msg:`カラム数は ${cols} だと判明しました。`, flag };
    },
    hint:'1から順に増やしていき、エラーが出る一歩手前の数字が答えです(二分探索でも構いません)。',
    explain:`ORDER BY の指定番号が実際のカラム数を超えるとエラーになります。エラーが出ない最大の番号が、そのテーブルのカラム数です。`
  };
}

function genF_union(rng){
  const t = pick(rng, DOC_POOL);
  const target = pick(rng, TABLE_POOL);
  const uCol = pick(rng, USERNAME_COLS), pCol = pick(rng, PASSWORD_COLS);
  const cols = randInt(rng, 3, 7);
  const flag = `FLAG{union_extraction_${randHex(rng,4)}}`;
  const users = shuffled(rng, NORMAL_USERS).slice(0,3).map(u=>({[uCol]:u, [pCol]:randPass(rng)}));
  return {
    title:'UNION SELECT でデータ抽出', tag:'intermediate',
    story:[
      `この${t.jp}検索(<code>${t.table}</code>、カラム数 ${cols})は <code>id</code> パラメータを直接埋め込んでいます。`,
      `UNION SELECT を使って、別テーブル <code>${target.table}</code> の ${uCol} と ${pCol} を抜き出してください。`,
      'ヒント: 前段階として ORDER BY でカラム数を数えておく必要があります。'
    ],
    queryTpl:(payload)=>`SELECT ${Array.from({length:cols},(_,i)=>'col'+(i+1)).join(',')} FROM ${t.table} WHERE id = 0 ${esc(payload)};`,
    fields:[{key:'payload', label:'追加のSQL(UNION以降)'}],
    check(vals){
      const p = vals.payload || '';
      const m = /union\s+select\s+(.+?)\s+from\s+(\w+)/i.exec(p);
      if (!m) return { ok:false, msg:'UNION SELECT の構文が見つかりません。「UNION SELECT 列1,列2,... FROM テーブル名」の形にしてください。' };
      const c = m[1].split(',').map(s=>s.trim());
      if (c.length !== cols) return { ok:false, msg:`データベースエラー: SELECT の列数(${c.length})が元のクエリの列数(${cols})と一致しません。` };
      if (!new RegExp(target.table,'i').test(m[2])) return { ok:false, msg:`テーブル "${m[2]}" は想定と異なります。${target.table} テーブルを指定してみてください。` };
      if (!new RegExp(uCol,'i').test(p) || !new RegExp(pCol,'i').test(p)) return { ok:false, msg:`${uCol} と ${pCol} の列を選択してください。` };
      const rowsHtml = users.map(u=>`<tr><td>${esc(u[uCol])}</td><td>${esc(u[pCol])}</td></tr>`).join('');
      return { ok:true, msg:`${target.table} テーブルの内容が漏洩しました:`, flag, extraHtml:`<table class="leak-table"><tr><th>${uCol}</th><th>${pCol}</th></tr>${rowsHtml}</table>` };
    },
    hint:`カラム数を${cols}個に揃え、狙った位置に ${uCol}, ${pCol} を、残りは適当な値で埋めましょう。`,
    explain:`UNION SELECT は2つのSELECT結果を縦に結合します。列数を元のクエリに合わせる必要があるため、事前にカラム数を調べておくことが重要です。`
  };
}

function genG_blind(rng){
  const t = pick(rng, TABLE_POOL);
  const pCol = pick(rng, PASSWORD_COLS);
  const secretLen = randInt(rng, 6, 14);
  const flag = `FLAG{blind_boolean_${randHex(rng,4)}}`;
  const adminRow = { username:'admin', [pCol]: 'x'.repeat(secretLen) };
  return {
    title:'ブラインドSQLインジェクション', tag:'advanced',
    story:[
      `${t.jp}テーブルへのログインフォームは結果を表示せず、「成立」「不成立」のどちらかだけを返します。`,
      `画面には何も表示されない状態で、admin の ${pCol} の文字数を当ててください。`,
      `(ヒント: <code>LENGTH(${pCol})</code> が使えます)`
    ],
    queryTpl:(cond)=>`SELECT * FROM ${t.table} WHERE username = 'admin' AND ${esc(cond)};`,
    fields:[{key:'cond', label:'真偽を確認したい条件'}],
    check(vals){
      const truthy = evaluateClause(`username = 'admin' AND ${vals.cond}`, adminRow);
      if (!truthy) return { ok:false, msg:'条件不成立 (false)' };
      const re = new RegExp(`length\\s*\\(\\s*${pCol}\\s*\\)\\s*=\\s*(\\d+)`, 'i');
      const m = re.exec(vals.cond);
      if (m && parseInt(m[1],10) === secretLen) return { ok:true, msg:`条件成立 (true)。${pCol} の長さは ${secretLen} 文字だと判明しました。`, flag };
      return { ok:false, msg:'条件成立 (true) — 画面上の反応から真偽だけがわかります。数字を変えて絞り込んでいきましょう。' };
    },
    hint:`LENGTH(${pCol})=1, =2, =3 ... と数を変えて送るうち、trueになる数字が見つかります。二分探索が効率的です。`,
    explain:`表示内容が同じでも「成立/不成立」の1ビットの差だけを頼りに、真偽値を1問1問聞き出してデータを推測するのがブラインドSQLインジェクションです。`
  };
}

function genH_filter(rng){
  const t = pick(rng, TABLE_POOL);
  const uCol = pick(rng, USERNAME_COLS), pCol = pick(rng, PASSWORD_COLS);
  const others = shuffled(rng, NORMAL_USERS).slice(0,2);
  const rows = [
    { [uCol]:'admin', [pCol]:randPass(rng) },
    { [uCol]:others[0], [pCol]:randPass(rng) },
    { [uCol]:others[1], [pCol]:randPass(rng) },
  ];
  const flag = `FLAG{filter_bypass_${randHex(rng,4)}}`;
  return {
    title:'フィルターバイパス', tag:'advanced',
    story:[
      '開発者はセキュリティ対策として、入力から "or" という文字列(大文字小文字問わず)を単純に削除するようにしました。',
      'それでもログイン認証をバイパスしてください。'
    ],
    queryTpl:(u,p)=>{
      const filtered = String(u).replace(/or/gi,'');
      return `-- フィルター適用後: '${esc(filtered)}'\nSELECT * FROM ${t.table} WHERE ${uCol} = '${esc(filtered)}' AND ${pCol} = '${esc(p)}';`;
    },
    fields:[{key:'u', label:'ユーザー名'}, {key:'p', label:'パスワード'}],
    check(vals){
      const filtered = String(vals.u).replace(/or/gi,'');
      const row = firstMatch(`${uCol} = '${filtered}' AND ${pCol} = '${vals.p}'`, rows);
      if (row) return { ok:true, msg:`フィルターを回避してログイン成功: ${row[uCol]}`, flag };
      return { ok:false, msg:`フィルター適用後の入力では一致しませんでした(適用後: "${filtered}")。` };
    },
    hint:'このフィルターは一回きりの単純な置換です。削除された後に "or" が"再構成"されるような書き方はできないでしょうか。また、この後ろには password のチェックも続いているので、Lv.2で使ったコメントのテクニックも組み合わせる必要があります。',
    explain:`<code>replace(/or/gi,'')</code> は1回だけスキャンして "or" を取り除きます。<code>oOrR</code> の中央2文字が消えると前後がくっついて実質 "or" が復元されてしまいます。さらに、その後ろに残る password のチェックを無効化するため、コメント(<code>--</code>)も併用しています。`
  };
}

function genI_unionComment(rng){
  const t = pick(rng, PRODUCT_POOL);
  const secretTable = pick(rng, SECRET_TABLE_POOL);
  const guardCol = pick(rng, ['status','visibility','state']);
  const guardVal = pick(rng, ['active','published','live']);
  const cols = randInt(rng, 3, 6);
  const flag = `FLAG{union_plus_comment_${randHex(rng,4)}}`;
  return {
    title:'UNION + コメントで機密情報を抜く', tag:'advanced',
    story:[
      `${t.jp}検索(${cols}カラム)の末尾には <code>AND ${guardCol} = '${guardVal}'</code> が付いています。`,
      `別テーブル <code>${secretTable}</code> に社外秘のフラグが保管されています。`,
      'UNION SELECT でその内容を抜き出し、末尾の条件も無効化してください。'
    ],
    queryTpl:(payload)=>`SELECT ${Array.from({length:cols},(_,i)=>'col'+(i+1)).join(',')} FROM ${t.table} WHERE id = 0 ${esc(payload)} AND ${guardCol} = '${guardVal}';`,
    fields:[{key:'payload', label:'追加のSQL'}],
    check(vals){
      const p = vals.payload || '';
      const m = /union\s+select\s+(.+?)\s+from\s+(\w+)/i.exec(p);
      if (!m) return { ok:false, msg:'UNION SELECT の構文が見つかりません。' };
      const c = m[1].split(',').map(s=>s.trim());
      if (c.length !== cols) return { ok:false, msg:`列数(${c.length})が元のクエリ(${cols}列)と一致しません。` };
      if (!new RegExp(secretTable,'i').test(m[2])) return { ok:false, msg:`${secretTable} テーブルを指定してみてください。` };
      const hasComment = /--|#/.test(p.slice(p.toLowerCase().indexOf('from')));
      if (!hasComment) return { ok:false, msg:`データベースエラー: UNIONの行に ${guardCol} カラムが存在しないため、末尾の条件でエラーになりました。末尾ごと無効化する方法を考えてみましょう。` };
      return { ok:true, msg:`${secretTable} テーブルの内容が漏洩しました。`, flag };
    },
    hint:`UNION部分の列数を${cols}つに揃え、テーブル名を ${secretTable} に。最後に -- を置いて元のクエリの残りを無効化しましょう。`,
    explain:`UNIONで持ってきた行には ${guardCol} 列が存在しないため、末尾の条件が残っているとエラーになります。<code>--</code> でその部分ごとコメントアウトして初めて成功します。`
  };
}

function genJ_timeBased(rng){
  const t = pick(rng, PRODUCT_POOL);
  const threshold = randInt(rng, 3, 6);
  const flag = `FLAG{time_based_blind_${randHex(rng,4)}}`;
  return {
    title:'時間ベース・ブラインド', tag:'advanced',
    story:[
      `${t.jp}情報の取得エンドポイントは、真でも偽でも画面上の表示が全く同じです。`,
      '唯一の手がかりは応答時間です。',
      `SLEEP() を使って、応答を${threshold}秒以上意図的に遅延させ、注入が効いていることを確認してください。`
    ],
    queryTpl:(payload)=>`SELECT * FROM ${t.table} WHERE id = ${esc(payload)};`,
    fields:[{key:'payload', label:'ID パラメータ'}],
    check(vals){
      const m = /sleep\s*\(\s*(\d+(?:\.\d+)?)\s*\)/i.exec(vals.payload || '');
      if (!m) return { ok:false, msg:'即座に応答が返りました(0ms)。SLEEP() を使った遅延を試してみましょう。' };
      const n = parseFloat(m[1]);
      if (n < threshold) return { ok:false, msg:`応答時間: 約${Math.round(n*200)}ms — 短すぎて確信を持てません。もう少し長い秒数を指定してみましょう。`, delayMs: n*200 };
      return { ok:true, msg:`応答時間: 約${Math.min(n,5)*1000}ms かかりました。意図的な遅延が発生 = SQLインジェクションが効いている証拠です。`, flag, delayMs: Math.min(n,5)*1000 };
    },
    hint:`例: 1 AND SLEEP(${threshold}) のように、真になる条件の後ろに SLEEP(秒数) を続けてみましょう。`,
    explain:`表示内容に差が出ない場合でも、SLEEP()による人為的な遅延を仕込み、応答時間を計測することで真偽を1ビットずつ読み取れます。これが時間ベース・ブラインドSQLインジェクションです。`
  };
}

function genK_stacked(rng){
  const t = pick(rng, PRODUCT_POOL);
  const priceCol = pick(rng, ['price','stock','discount']);
  const newVal = randInt(rng, 0, 1);
  const flag = `FLAG{stacked_query_${randHex(rng,4)}}`;
  return {
    title:'スタックドクエリ', tag:'advanced',
    story:[
      `${t.jp}検索は複数のSQL文をセミコロンで区切って一度に実行できるDB設定になっています(一部のドライバ/DBで見られる設定です)。`,
      `1つのクエリの結果を返すだけでなく、続けて別の文を実行して ${priceCol} を書き換えてください。`
    ],
    queryTpl:(payload)=>`SELECT * FROM ${t.table} WHERE id = ${esc(payload)};`,
    fields:[{key:'payload', label:'ID パラメータ'}],
    check(vals){
      const p = vals.payload || '';
      const re = new RegExp(`;\\s*update\\s+${t.table}\\s+set\\s+${priceCol}\\s*=\\s*${newVal}`, 'i');
      if (!re.test(p)) return { ok:false, msg:'追加の文が実行された形跡がありません。セミコロンで文を区切り、UPDATE文を続けてみましょう。' };
      const hasTerminator = /--|#/.test(p);
      if (!hasTerminator) return { ok:false, msg:'2つ目の文の後ろにも元の文の残りが続いてしまい、構文エラーになりました。コメントで打ち切りましょう。' };
      return { ok:true, msg:`UPDATE文が実行され、${t.table}.${priceCol} が ${newVal} に書き換わりました。`, flag };
    },
    hint:`例: 1; UPDATE ${t.table} SET ${priceCol}=${newVal} -- のように、セミコロンで文を区切って新しい文を続けます。`,
    explain:`一部のDB接続方式は、1回の呼び出しで複数のSQL文をセミコロン区切りでまとめて実行できます。これを悪用し、元のSELECTとは別に任意のUPDATE/DELETE/INSERTなどを注入する手法をスタックドクエリと呼びます。`
  };
}

function genL_secondOrder(rng){
  const t = pick(rng, TABLE_POOL);
  const flag = `FLAG{second_order_${randHex(rng,4)}}`;
  return {
    title:'セカンドオーダーインジェクション', tag:'advanced',
    story:[
      `${t.jp}の「表示名」欄への入力自体は、そのまま安全に保存されます(この時点ではエスケープも正しく行われます)。`,
      '問題は後日: 管理者向けの利用ログ集計スクリプトが、この表示名を未対策のまま別のクエリに埋め込んで実行することです。',
      '表示名を今登録しておき、後から実行される集計クエリの方をバイパスしてください。'
    ],
    queryTpl:(name)=>`-- 1) 登録時(安全)\nINSERT INTO ${t.table} (display_name) VALUES ('${esc(name)}');\n\n-- 2) 後日、管理者ログ集計スクリプトが実行するクエリ(対策なし)\nSELECT * FROM admin_logs WHERE actor = '${esc(name)}';`,
    fields:[{key:'name', label:'表示名(登録内容)'}],
    check(vals){
      const clause = `actor = '${vals.name}'`;
      const hit = evaluateClause(clause, { actor:'system' });
      if (hit) return { ok:true, msg:'後日実行された集計クエリの条件が常に真になり、本来見えないログまで対象になりました。', flag };
      return { ok:false, msg:'集計クエリの条件は成立しませんでした。' };
    },
    hint:'登録した時点ではエスケープされていても、後で別の(対策されていない)クエリに生の文字列として使われるなら、通常のOR/コメント技術がそのまま効きます。',
    explain:`入力を保存した時点では安全でも、後で別の処理が同じ値を未対策のクエリに使い回すと、そこで初めて攻撃が発火します。これをセカンドオーダーインジェクションと呼び、"入力元"だけを対策していても防げない典型例です。`
  };
}

function genM_wafCaseBypass(rng){
  const t = pick(rng, DOC_POOL);
  const target = pick(rng, SECRET_TABLE_POOL);
  const cols = randInt(rng, 3, 5);
  const flag = `FLAG{waf_case_bypass_${randHex(rng,4)}}`;
  return {
    title:'大文字小文字によるWAFバイパス', tag:'advanced',
    story:[
      `手前段にWAF(Web Application Firewall)があり、大文字の "UNION SELECT" という文字列を検知すると通信をブロックします。`,
      `SQL自体は大文字・小文字を区別しないため、WAFの検知パターンだけをすり抜ける書き方を考えてください。`,
      `目標: ${target} テーブルの内容を UNION で抜き出してください(カラム数は${cols})。`
    ],
    queryTpl:(payload)=>`SELECT ${Array.from({length:cols},(_,i)=>'col'+(i+1)).join(',')} FROM ${t.table} WHERE id = 0 ${esc(payload)};`,
    fields:[{key:'payload', label:'追加のSQL'}],
    check(vals){
      const p = vals.payload || '';
      if (/UNION SELECT/.test(p)) return { ok:false, msg:'🛑 WAFにブロックされました("UNION SELECT" という大文字の並びを検知)。大文字・小文字を混ぜて検知パターンを回避してみましょう。' };
      const m = /union\s+select\s+(.+?)\s+from\s+(\w+)/i.exec(p);
      if (!m) return { ok:false, msg:'UNION SELECT の構文が見つかりません。' };
      const c = m[1].split(',').map(s=>s.trim());
      if (c.length !== cols) return { ok:false, msg:`列数(${c.length})が元のクエリ(${cols}列)と一致しません。` };
      if (!new RegExp(target,'i').test(m[2])) return { ok:false, msg:`${target} テーブルを指定してみてください。` };
      return { ok:true, msg:`WAFを回避しつつ ${target} の内容を抜き出しました。`, flag };
    },
    hint:'例: uNioN SeLeCt のように大文字・小文字を混在させます。SQLエンジン自体は大文字小文字を区別しないので、これでも正しく実行されます。',
    explain:`多くのWAFは正規表現などの完全一致・大文字一致パターンで危険な文字列を検知します。しかしSQL自体は大文字小文字を区別しないため、"UnIoN SeLeCt" のように表記を変えるだけで検知ルールをすり抜けつつ、DB側では正常に解釈されてしまいます。`
  };
}

function genN_headerInjection(rng){
  const flag = `FLAG{header_injection_${randHex(rng,4)}}`;
  const secretRow = { agent:'internal-admin-crawler/1.0', note:'社内限定ログ' };
  return {
    title:'HTTPヘッダー経由のインジェクション', tag:'intermediate',
    story:[
      'このサイトはアクセスログを記録する際、フォームの入力欄ではなく "User-Agent" ヘッダーの値をそのままSQLに埋め込んでいます。',
      '画面上の入力フォームは存在しませんが、User-Agentの値は自由に偽装して送信できます。',
      '本来は見えないはずの社内限定ログを表示させてください。'
    ],
    queryTpl:(agent)=>`SELECT * FROM access_logs WHERE agent = '${esc(agent)}';`,
    fields:[{key:'agent', label:'User-Agent(偽装して送信する値)'}],
    check(vals){
      const hit = evaluateClause(`agent = '${vals.agent}'`, secretRow);
      if (hit) return { ok:true, msg:'社内限定ログが表示されました。', flag };
      return { ok:false, msg:'該当するログはありませんでした。' };
    },
    hint:'フォームの入力欄でなくても、サーバーが信用してそのままSQLに使っている値であれば、同じ手口(OR/コメント)が通用します。',
    explain:`SQLインジェクションはフォームの入力欄だけの問題ではありません。User-Agent、RefererなどのHTTPヘッダー、Cookie、URLパラメータなど、サーバーが受け取ってSQLに使う値はすべて攻撃対象になり得ます。`
  };
}

function genO_likeWildcard(rng){
  const t = pick(rng, PRODUCT_POOL);
  const hiddenName = 'internal-'+randHex(rng,6);
  const flag = `FLAG{like_wildcard_${randHex(rng,4)}}`;
  return {
    title:'LIKE演算子のワイルドカード悪用', tag:'beginner',
    story:[
      `${t.jp}の部分一致検索は <code>LIKE</code> 演算子を使っています。`,
      '本来は名前の一部を入力する機能ですが、ワイルドカード自体や引用符の扱いに注意してください。',
      '検索条件を知らなくても、非公開のアイテムを検索結果に含めてください。'
    ],
    queryTpl:(q)=>`SELECT * FROM ${t.table} WHERE name LIKE '%${esc(q)}%';`,
    fields:[{key:'q', label:'部分一致検索'}],
    check(vals){
      const q = vals.q || '';
      if (q.includes("' OR '1'='1") || q.includes("' OR 1=1")) {
        return { ok:true, msg:`非公開アイテム「${hiddenName}」が検索結果に含まれました。`, flag };
      }
      return { ok:false, msg:'検索結果: 該当するアイテムはありませんでした。' };
    },
    hint:"検索欄は %入力%% のように % で囲まれて埋め込まれます。引用符を閉じてOR条件を足す発想は今までと同じです。例: ' OR '1'='1",
    explain:`LIKE検索であっても、入力値が引用符でそのまま囲まれて埋め込まれている以上、通常のORベースのバイパスがそのまま通用します。ワイルドカード自体(%, _)を悪用して全件を一致させる手口も、同じ発想の延長です。`
  };
}

genA_orBypass.tag='beginner'; genB_commentBypass.tag='beginner'; genC_numeric.tag='beginner'; genO_likeWildcard.tag='beginner';
genD_doubleQuote.tag='intermediate'; genE_orderBy.tag='intermediate'; genF_union.tag='intermediate'; genN_headerInjection.tag='intermediate';
genG_blind.tag='advanced'; genH_filter.tag='advanced'; genI_unionComment.tag='advanced'; genJ_timeBased.tag='advanced';
genK_stacked.tag='advanced'; genL_secondOrder.tag='advanced'; genM_wafCaseBypass.tag='advanced';

const FAMILIES = [
  genA_orBypass, genB_commentBypass, genC_numeric, genD_doubleQuote, genE_orderBy,
  genF_union, genG_blind, genH_filter, genI_unionComment, genJ_timeBased,
  genK_stacked, genL_secondOrder, genM_wafCaseBypass, genN_headerInjection, genO_likeWildcard,
];
const DAILY_COUNT = 10;

function buildBatch(seedStr){
  const rng = seededRng(seedStr);
  const tagOrder = { beginner:0, intermediate:1, advanced:2 };
  const chosen = shuffled(rng, FAMILIES)
    .slice(0, DAILY_COUNT)
    .sort((a,b)=> tagOrder[a.tag] - tagOrder[b.tag]);
  return chosen.map((fn,i)=>{ const lv = fn(rng); lv.id = i+1; lv._fn = fn; return lv; });
}
function seedForYMD(y, m, d){
  return `sqli-dojo-${y}-${m}-${d}`;
}
function todaySeed(){
  const d = new Date();
  return seedForYMD(d.getFullYear(), d.getMonth()+1, d.getDate());
}
function ymdInputValue(d){
  const pad=n=>String(n).padStart(2,'0');
  return `${d.getFullYear()}-${pad(d.getMonth()+1)}-${pad(d.getDate())}`;
}
const WEEKDAY_JP = ['日','月','火','水','木','金','土'];
function formatDateJP(d, isToday){
  return `${d.getFullYear()}年${d.getMonth()+1}月${d.getDate()}日(${WEEKDAY_JP[d.getDay()]})${isToday ? ' — 本日' : ''}`;
}

/* ---------- Firebase(共通アカウント・進捗保存) ---------- */
// SQLインジェクション道場専用のFirebaseプロジェクト(sqlinjection-47b37)。
const firebaseConfig = {
  apiKey: "AIzaSyBZKRiBaEaGwuKV9lA9T_b_viO0t0XakHU",
  authDomain: "sqlinjection-47b37.firebaseapp.com",
  projectId: "sqlinjection-47b37",
  storageBucket: "sqlinjection-47b37.firebasestorage.app",
  messagingSenderId: "534699590251",
  appId: "1:534699590251:web:143f92fa22917f8c8e272e",
};
if (!firebase.apps.length) firebase.initializeApp(firebaseConfig);
const db = firebase.firestore();
const auth = firebase.auth();
auth.setPersistence(firebase.auth.Auth.Persistence.NONE).catch(e=>console.error('persistence設定に失敗', e));
// ↑ ログイン状態をブラウザに保存しない設定。これにより、ページを開くたびに必ずログイン画面が表示される。
const USERS_COLLECTION = 'users';
const LEADERBOARD_COLLECTION = 'leaderboard'; // username と累計日数だけを持つ(isAdminなどは含めない)
const EMAIL_DOMAIN = 'sqli-dojo.local'; // userIDをFirebase Auth用のダミーメールに変換するためのドメイン

function usernameToEmail(u){ return u.toLowerCase() + '@' + EMAIL_DOMAIN; }
const USERNAME_RULE = /^[A-Za-z0-9_-]{3,20}$/;
const PASSWORD_RULE = /^(?=.*[0-9])(?=.*[A-Za-z])[A-Za-z0-9]{8,}$/;
const PW_HAS_NUM = /[0-9]/, PW_HAS_ALPHA = /[A-Za-z]/, PW_CHARSET = /^[A-Za-z0-9]*$/;

let currentUser = null; // { uid, username, streak, lastCompletedDate, completedDates:Set, isAdmin, createdAt }
let isGuest = false;
let isRegistering = false; // 登録処理中はonAuthStateChangedの自動反応を止め、画面のちらつきを防ぐ

async function loadUserDoc(uid, username){
  const doc = await db.collection(USERS_COLLECTION).doc(uid).get();
  const data = doc.exists ? doc.data() : {};
  currentUser = {
    uid, username,
    streak: data.streak || 0,
    lastCompletedDate: data.lastCompletedDate || null,
    completedDates: new Set(Object.keys(data.completedDates || {})),
    isAdmin: !!data.isAdmin,
    createdAt: data.createdAt || null,
  };
}
async function saveUserDoc(){
  if (!currentUser) return;
  const completedDatesObj = {};
  currentUser.completedDates.forEach(k=> completedDatesObj[k] = true);
  await db.collection(USERS_COLLECTION).doc(currentUser.uid).set({
    username: currentUser.username,
    streak: currentUser.streak,
    lastCompletedDate: currentUser.lastCompletedDate,
    completedDates: completedDatesObj,
  }, { merge:true });
}

// ランキング専用コレクションへの同期。username と累計日数(totalDays)だけを書き込み、
// isAdmin などの情報は一切含めないため、ランキングを見ても管理者かどうかは分からない。
async function syncLeaderboard(){
  if (!currentUser) return;
  await db.collection(LEADERBOARD_COLLECTION).doc(currentUser.uid).set({
    username: currentUser.username,
    totalDays: currentUser.completedDates.size,
  });
}

function dateKeyOf(y,m,d){ return `${y}-${m}-${d}`; }
function yesterdayKeyOf(y,m,d){
  const dt = new Date(y, m-1, d);
  dt.setDate(dt.getDate()-1);
  return dateKeyOf(dt.getFullYear(), dt.getMonth()+1, dt.getDate());
}

// 日付プレイ(state.dateKey)の10問すべてをクリアし、かつそれが「今日」で
// あった場合にのみ、連続記録(ストリーク)を更新する。
async function markDailyCompleteIfNeeded(){
  if (!currentUser || state.mode !== 'daily') return;
  const today = new Date();
  const todayKey = dateKeyOf(today.getFullYear(), today.getMonth()+1, today.getDate());
  if (state.dateKey !== todayKey) return; // 過去の日付を後から解いた場合はストリーク対象外
  if (currentUser.completedDates.has(todayKey)) return; // 既に記録済み

  currentUser.completedDates.add(todayKey);
  const yKey = yesterdayKeyOf(today.getFullYear(), today.getMonth()+1, today.getDate());
  if (currentUser.lastCompletedDate === yKey) currentUser.streak += 1;
  else currentUser.streak = 1;
  currentUser.lastCompletedDate = todayKey;
  renderUserBar();
  try {
    await saveUserDoc();
    await syncLeaderboard();
  } catch(e){ console.error('保存に失敗しました', e); }
}

function renderUserBar(){
  const wrap = document.getElementById('userBarWrap');
  const adminWrap = document.getElementById('adminBtnWrap');
  if (!currentUser){
    wrap.style.display='none'; wrap.innerHTML='';
    if (isGuest){
      wrap.style.display='';
      wrap.innerHTML = `
        <div class="guest-bar">
          <span>🕶 ゲストモード(記録は保存されません)</span>
          <button id="guestBackBtn">ログイン画面に戻る</button>
        </div>`;
      document.getElementById('guestBackBtn').onclick = ()=>{
        isGuest = false;
        renderUserBar();
        showScreen('loginScreen');
      };
    }
    adminWrap.style.display='none'; adminWrap.innerHTML='';
    return;
  }
  wrap.style.display='';
  wrap.innerHTML = `
    <div class="user-bar">
      <span>👤 ${esc(currentUser.username)}</span>
      <span class="streak-badge">🔥 連続 ${currentUser.streak} 日</span>
      <button id="rankBtn">🏆 ランキング</button>
      <button id="logoutBtn">ログアウト</button>
    </div>`;
  document.getElementById('logoutBtn').onclick = handleLogout;
  document.getElementById('rankBtn').onclick = renderRanking;

  if (currentUser.isAdmin){
    adminWrap.style.display='';
    adminWrap.innerHTML = `<button class="admin-btn" id="openAdminBtn">🛠 管理者パネル</button>`;
    document.getElementById('openAdminBtn').onclick = renderAdminPanel;
  } else {
    adminWrap.style.display='none'; adminWrap.innerHTML='';
  }
}

/* ---------- カスタムモーダル ---------- */
function showModal(title, message, buttons){
  // buttons: [{label, primary, onClick}]
  const overlay = document.getElementById('modalOverlay');
  document.getElementById('modalTitle').textContent = title;
  document.getElementById('modalMessage').textContent = message;
  const btnsEl = document.getElementById('modalBtns');
  btnsEl.innerHTML = '';
  buttons.forEach(b=>{
    const btn = document.createElement('button');
    btn.className = b.primary ? 'run' : 'ghost';
    btn.textContent = b.label;
    btn.onclick = ()=>{ overlay.classList.remove('active'); if (b.onClick) b.onClick(); };
    btnsEl.appendChild(btn);
  });
  overlay.classList.add('active');
}

/* ---------- 管理者パネル ---------- */
async function renderAdminPanel(){
  showScreen('adminScreen');
  const wrap = document.getElementById('adminTableWrap');
  wrap.innerHTML = '<p class="admin-sub">読み込み中...</p>';
  try {
    const snap = await db.collection(USERS_COLLECTION).get();
    const today = new Date();
    const todayKey = dateKeyOf(today.getFullYear(), today.getMonth()+1, today.getDate());
    const rows = [];
    snap.forEach(doc=>{
      const d = doc.data();
      const done = !!(d.completedDates && d.completedDates[todayKey]);
      const createdStr = d.createdAt && d.createdAt.toDate ? d.createdAt.toDate().toLocaleString('ja-JP') : '不明';
      rows.push({ uid:doc.id, username:d.username || '(不明)', done, createdStr });
    });
    rows.sort((a,b)=> a.username.localeCompare(b.username));
    const rowsHtml = rows.map(r=>`
      <tr>
        <td>${esc(r.username)}${r.uid===currentUser.uid ? '' : ` <button class="admin-del-btn" data-uid="${r.uid}">削除</button>`}</td>
        <td class="${r.done?'status-done':'status-undone'}">${r.done?'✓ 実施済み':'未実施'}</td>
      </tr>`).join('');
    wrap.innerHTML = `
      <table class="admin-table">
        <tr><th>ユーザーID</th><th>本日のデイリー</th></tr>
        ${rowsHtml || '<tr><td colspan="2">ユーザーがいません</td></tr>'}
      </table>
      <div style="margin-top:16px;"><button class="ghost" id="adminBackBtn">← 戻る</button></div>
      <p class="admin-sub" style="margin-top:14px;">※「削除」はこのユーザーの記録データ(Firestore)を削除します。ログイン用のアカウント自体(Firebase Authentication)はブラウザからは削除できないため、別途Cloud Functions等での対応が必要です。</p>
    `;
    document.getElementById('adminBackBtn').onclick = renderLanding;
    wrap.querySelectorAll('.admin-del-btn').forEach(btn=>{
      btn.onclick = ()=>{
        const uid = btn.dataset.uid;
        const username = rows.find(r=>r.uid===uid).username;
        showModal('削除の確認', `${username} のデータを削除します。元に戻せません。よろしいですか?`, [
          { label:'キャンセル', primary:false },
          { label:'削除する', primary:true, onClick: async ()=>{
              try { await db.collection(USERS_COLLECTION).doc(uid).delete(); renderAdminPanel(); }
              catch(e){ console.error(e); showModal('エラー', '削除に失敗しました。', [{label:'OK', primary:true}]); }
            } },
        ]);
      };
    });
  } catch(e){
    console.error(e);
    wrap.innerHTML = '<p class="admin-sub">読み込みに失敗しました。管理者権限があるか、Firestoreのルールをご確認ください。</p><button class="ghost" id="adminBackBtn2">← 戻る</button>';
    document.getElementById('adminBackBtn2').onclick = renderLanding;
  }
}

/* ---------- ランキング(累計日数) ---------- */
async function renderRanking(){
  showScreen('rankScreen');
  const wrap = document.getElementById('rankTableWrap');
  wrap.innerHTML = '<p class="admin-sub">読み込み中...</p>';
  try {
    const snap = await db.collection(LEADERBOARD_COLLECTION).get();
    const rows = [];
    snap.forEach(doc=>{
      const d = doc.data();
      rows.push({ uid:doc.id, username:d.username || '(不明)', totalDays:d.totalDays || 0 });
    });
    rows.sort((a,b)=> b.totalDays - a.totalDays);
    const rowsHtml = rows.map((r,i)=>{
      const isYou = currentUser && r.uid === currentUser.uid;
      return `<tr class="${isYou?'you-row':''}">
        <td>${i+1}</td>
        <td>${esc(r.username)}${isYou?' (あなた)':''}</td>
        <td>${r.totalDays} 日</td>
      </tr>`;
    }).join('');
    wrap.innerHTML = `
      <table class="admin-table">
        <tr><th>順位</th><th>ユーザーID</th><th>累計クリア日数</th></tr>
        ${rowsHtml || '<tr><td colspan="3">まだ誰もクリアしていません</td></tr>'}
      </table>
      <div style="margin-top:16px;"><button class="ghost" id="rankBackBtn">← 戻る</button></div>
    `;
    document.getElementById('rankBackBtn').onclick = renderLanding;
  } catch(e){
    console.error(e);
    wrap.innerHTML = '<p class="admin-sub">読み込みに失敗しました。</p><button class="ghost" id="rankBackBtn2">← 戻る</button>';
    document.getElementById('rankBackBtn2').onclick = renderLanding;
  }
}

function showScreen(id){
  ['loginScreen','registerScreen','landing','appLayout','adminScreen','rankScreen'].forEach(s=>{
    document.getElementById(s).style.display = (s===id) ? '' : 'none';
  });
  document.getElementById('appHeaderControls').style.display = (id==='appLayout') ? '' : 'none';
}

/* ---- パスワードルールの動的表示 ---- */
function wirePasswordRuleUI(){
  const pwInput = document.getElementById('registerPassword');
  pwInput.addEventListener('input', ()=>{
    const pw = pwInput.value;
    const set=(id,ok)=>{ document.getElementById(id).classList.toggle('valid', ok); };
    set('ruleLength', pw.length>=8);
    set('ruleNumber', PW_HAS_NUM.test(pw));
    set('ruleAlpha', PW_HAS_ALPHA.test(pw));
    set('ruleCharset', pw.length>0 && PW_CHARSET.test(pw));
  });
}
document.querySelectorAll('.pw-toggle').forEach(btn=>{
  btn.onclick = ()=>{
    const input = document.getElementById(btn.dataset.target);
    if (input.type==='password'){ input.type='text'; btn.textContent='隠す'; }
    else { input.type='password'; btn.textContent='表示'; }
  };
});
wirePasswordRuleUI();

document.getElementById('toRegisterBtn').onclick = ()=> showScreen('registerScreen');
document.getElementById('toLoginBtn').onclick = ()=> showScreen('loginScreen');

document.getElementById('guestBtn').onclick = ()=>{
  showModal(
    'ゲストとして続ける',
    'ゲストのまま進めると、デイリー問題の実施記録や連続記録(ストリーク)は保存されません。よろしいですか?',
    [
      { label:'キャンセル', primary:false },
      { label:'ゲストとして続ける', primary:true, onClick: ()=>{
          currentUser = null;
          isGuest = true;
          renderUserBar();
          renderLanding();
        } },
    ]
  );
};

document.getElementById('loginBtn').onclick = async ()=>{
  const uid = document.getElementById('loginUserId').value.trim();
  const pw = document.getElementById('loginPassword').value;
  const errEl = document.getElementById('loginError');
  errEl.textContent = '';
  if (!uid || !pw){ errEl.textContent = 'ユーザーIDとパスワードを入力してください。'; return; }
  const btn = document.getElementById('loginBtn');
  btn.disabled = true; btn.textContent = 'ログイン中...';
  try {
    const cred = await auth.signInWithEmailAndPassword(usernameToEmail(uid), pw);
    await loadUserDoc(cred.user.uid, uid);
    isGuest = false;
    renderUserBar();
    renderLanding();
  } catch(e){
    console.error(e);
    errEl.textContent = 'ユーザーIDまたはパスワードが正しくありません。';
  } finally {
    btn.disabled = false; btn.textContent = 'ログイン';
  }
};

document.getElementById('registerBtn').onclick = async ()=>{
  const uid = document.getElementById('registerUserId').value.trim();
  const pw = document.getElementById('registerPassword').value;
  const errEl = document.getElementById('registerError');
  errEl.textContent = '';
  if (!USERNAME_RULE.test(uid)){ errEl.textContent = 'ユーザーIDは半角英数字と_-のみ、3〜20文字にしてください。'; return; }
  if (!PASSWORD_RULE.test(pw)){ errEl.textContent = 'パスワードの条件を満たしていません。'; return; }
  const btn = document.getElementById('registerBtn');
  btn.disabled = true; btn.textContent = '登録中...';
  isRegistering = true;
  try {
    const cred = await auth.createUserWithEmailAndPassword(usernameToEmail(uid), pw);
    await db.collection(USERS_COLLECTION).doc(cred.user.uid).set({
      username: uid, streak:0, lastCompletedDate:null, completedDates:{},
      isAdmin: false, createdAt: firebase.firestore.FieldValue.serverTimestamp(),
    });
    await auth.signOut();
    showScreen('loginScreen');
    document.getElementById('loginError').textContent = '';
    document.getElementById('loginUserId').value = uid;
    showModal('登録完了', '登録が完了しました。ログインしてください。', [{ label:'OK', primary:true }]);
  } catch(e){
    console.error(e);
    if (e.code === 'auth/email-already-in-use') errEl.textContent = 'そのユーザーIDは既に使用されています。';
    else errEl.textContent = '登録に失敗しました。通信環境をご確認ください。';
  } finally {
    btn.disabled = false; btn.textContent = '登録する';
    isRegistering = false;
  }
};

function handleLogout(){
  auth.signOut();
  currentUser = null;
  renderUserBar();
  showScreen('loginScreen');
}

/* ---------- 状態管理 ---------- */
let LEVELS = buildBatch(todaySeed());
const state = { current:1, cleared:new Set(), mode:'daily', dateKey:null };

function renderLevelList(){
  const ol=document.getElementById('levelList');
  ol.innerHTML='';
  LEVELS.forEach(lv=>{
    const li=document.createElement('li');
    const btn=document.createElement('button');
    btn.className='lvl-btn'+(lv.id===state.current?' active':'')+(state.cleared.has(lv.id)?' cleared':'');
    btn.innerHTML=`<span class="level-num">${String(lv.id).padStart(2,'0')}</span><span class="lvl-title">${lv.title}</span><span class="tag ${lv.tag}">${tagLabel(lv.tag)}</span>`;
    btn.onclick=()=>{ state.current=lv.id; render(); };
    li.appendChild(btn);
    ol.appendChild(li);
  });
  document.getElementById('clearedCount').textContent = state.cleared.size;
}
function tagLabel(t){ return t==='beginner'?'初級':t==='intermediate'?'中級':'上級'; }

function render(){
  renderLevelList();
  const lv = LEVELS.find(l=>l.id===state.current);
  const panel = document.getElementById('panel');

  const fieldsHtml = lv.fields.map(f=>`
    <div>
      <label for="f_${f.key}">${f.label}</label>
      <input type="text" id="f_${f.key}" autocomplete="off" spellcheck="false">
    </div>`).join('');

  const initialVals = {};
  lv.fields.forEach(f=> initialVals[f.key] = '');
  const initialQuery = lv.queryTpl(...lv.fields.map(f=>initialVals[f.key] || '???'));

  const shuffleBtnHtml = state.mode==='free'
    ? `<button class="shuffle-one" id="shuffleOneBtn">🔀 別パターンに入れ替える</button>`
    : '';

  panel.innerHTML = `
    <div class="panel-head">
      <h2>Lv.${lv.id} ${lv.title}</h2>
      <span class="tag ${lv.tag}">${tagLabel(lv.tag)}</span>
    </div>
    <div class="scenario">${lv.story.map(p=>`<p>${p}</p>`).join('')}</div>
    <div class="query-box" id="queryBox">${esc(initialQuery)}</div>
    <fieldset>${fieldsHtml}</fieldset>
    <div class="actions">
      <button class="run" id="runBtn">実行</button>
      <button class="ghost" id="hintBtn">ヒントを見る</button>
      ${shuffleBtnHtml}
    </div>
    <div id="hintArea"></div>
    <div id="resultArea"></div>
  `;

  if (state.mode==='free'){
    document.getElementById('shuffleOneBtn').onclick = ()=>{
      const idx = LEVELS.findIndex(l=>l.id===state.current);
      const fn = LEVELS[idx]._fn || FAMILIES[0];
      const fresh = fn(seededRng('shuffle-'+Date.now()+'-'+Math.random()));
      fresh.id = LEVELS[idx].id;
      fresh._fn = fn;
      LEVELS[idx] = fresh;
      state.cleared.delete(fresh.id);
      render();
    };
  }

  lv.fields.forEach(f=>{
    document.getElementById('f_'+f.key).addEventListener('input', updateQueryPreview);
  });
  function updateQueryPreview(){
    const vals = lv.fields.map(f=> document.getElementById('f_'+f.key).value || '???');
    document.getElementById('queryBox').textContent = lv.queryTpl(...vals);
  }

  document.getElementById('hintBtn').onclick = ()=>{
    document.getElementById('hintArea').innerHTML = `<div class="hint-box">💡 ${lv.hint}</div>`;
  };

  document.getElementById('runBtn').onclick = ()=>{
    const vals = {};
    lv.fields.forEach(f=> vals[f.key] = document.getElementById('f_'+f.key).value);
    const resultArea = document.getElementById('resultArea');
    resultArea.innerHTML = `<div class="result">実行中...</div>`;
    const finish = ()=>{
      const res = lv.check(vals);
      let html = `<div class="result ${res.ok?'ok':'fail'}">${esc(res.msg)}`;
      if (res.flag) html += `<div class="flag">${esc(res.flag)}</div>`;
      if (res.extraHtml) html += res.extraHtml;
      html += `</div>`;
      if (res.ok){
        state.cleared.add(lv.id);
        html += `<div class="explain-box"><h4>種明かし</h4>${lv.explain}</div>`;
        renderLevelList();
        if (state.cleared.size >= LEVELS.length){
          if (state.mode==='daily'){
            markDailyCompleteIfNeeded();
          } else if (state.mode==='free'){
            html += `<div class="freeplay-more"><p>この${LEVELS.length}問をすべてクリアしました!まだ続けますか?</p><button class="run" id="moreBtn">➕ さらに10問追加する</button></div>`;
          }
        }
      }
      resultArea.innerHTML = html;
      if (state.mode==='free' && state.cleared.size >= LEVELS.length){
        const moreBtn = document.getElementById('moreBtn');
        if (moreBtn) moreBtn.onclick = loadMoreFreePractice;
      }
    };
    if (vals.payload !== undefined || lv.fields.some(f=>f.key==='payload')){
      const delay = (lv.check(vals).delayMs) || 0;
      setTimeout(finish, Math.min(delay, 5000));
    } else {
      finish();
    }
  };

  updateQueryPreview();
}

function loadMoreFreePractice(){
  const startId = LEVELS.length + 1;
  const extra = buildBatch('shuffle-'+Date.now()+'-'+Math.random());
  extra.forEach((lv,i)=>{ lv.id = startId + i; LEVELS.push(lv); });
  state.current = startId;
  render();
}

const RELEASE_DATE = new Date(2026, 8, 22); // 本日リリース。日付リストはこの日から今日までを表示する。

function renderLanding(){
  showScreen('landing');

  const today = new Date();
  const diffDays = Math.max(0, Math.round((stripTime(today) - stripTime(RELEASE_DATE)) / 86400000));
  const list = document.getElementById('dateList');
  list.innerHTML = '';

  const freeLi = document.createElement('li');
  const freeBtn = document.createElement('button');
  freeBtn.className = 'date-item freeplay';
  freeBtn.innerHTML = `<span>🎲 自由練習(無制限)<br><span class="sub">デイリーで飽き足りない人向け。10問終わるとさらに10問追加できます。</span></span><span class="arrow">›</span>`;
  freeBtn.onclick = ()=> enterApp('shuffle-'+Date.now()+'-'+Math.random(), '自由練習', 'free', null);
  freeLi.appendChild(freeBtn);
  list.appendChild(freeLi);

  for (let i=0;i<=diffDays;i++){
    const d = new Date(today);
    d.setDate(today.getDate()-i);
    const key = dateKeyOf(d.getFullYear(), d.getMonth()+1, d.getDate());
    const isToday = i===0;
    const done = currentUser && currentUser.completedDates.has(key);
    const li = document.createElement('li');
    const btn = document.createElement('button');
    btn.className = 'date-item'+(isToday?' today':'');
    const statusHtml = currentUser
      ? (done ? '<span class="done-mark">✓ 実施済み</span>' : '<span class="undone-mark">未実施</span>')
      : '';
    btn.innerHTML = `<span>${formatDateJP(d, isToday)}<br><span class="sub">${isToday?'本日配信の10問':'この日の10問'}</span></span>${statusHtml}<span class="arrow">›</span>`;
    btn.onclick = ()=> enterApp(seedForYMD(d.getFullYear(), d.getMonth()+1, d.getDate()), formatDateJP(d, isToday), 'daily', key);
    li.appendChild(btn);
    list.appendChild(li);
  }
}
function stripTime(d){ return new Date(d.getFullYear(), d.getMonth(), d.getDate()).getTime(); }

function enterApp(seed, label, mode, dateKey){
  LEVELS = buildBatch(seed);
  state.cleared.clear();
  state.current = 1;
  state.mode = mode;
  state.dateKey = dateKey;
  showScreen('appLayout');
  render();
}

document.getElementById('backBtn').onclick = renderLanding;

// 初期状態: 未ログインならログイン画面、Firebase Authが既存セッションを
// 検知した場合はそのままランディング(日付一覧)へ。
auth.onAuthStateChanged(async (user)=>{
  if (isRegistering) return; // 登録処理中の自動サインインイベントは無視する
  if (user && !currentUser){
    try {
      const username = user.email.split('@')[0];
      await loadUserDoc(user.uid, username);
      renderUserBar();
      renderLanding();
    } catch(e){
      console.error(e);
      showScreen('loginScreen');
    }
  } else if (!user && !currentUser) {
    showScreen('loginScreen');
  }
});
})();
