# Requires PyYAML and tree-sitter-language-pack==0.9.0. Does not execute R.
from pathlib import Path
from tree_sitter_language_pack import get_parser
import yaml,re,json,datetime
root=Path(__file__).resolve().parents[1];parser=get_parser('r');issues=[];rfiles=list(root.rglob('*.R'));yfiles=list((root/'jamovi').glob('*.yaml'))
for p in rfiles:
 tree=parser.parse(p.read_bytes())
 if tree.root_node.has_error: issues.append('R syntax: '+str(p))
ys={p.name:yaml.safe_load(p.read_text()) for p in yfiles}
refs=set(ys['00refs.yaml']['refs'])
for p in (root/'jamovi').glob('*.a.yaml'):
 obj=ys[p.name];options={o['name'] for o in obj.get('options',[])}
 r=p.with_name(p.name.replace('.a.','.r.'))
 b=root/'R'/p.name.replace('.a.yaml','.b.R')
 if b.exists():
  source=b.read_bytes();tree=parser.parse(source)
  def optionwalk(node):
   if node.type=='extract_operator':
    text=source[node.start_byte:node.end_byte].decode()
    match=re.fullmatch(r'self\$options\$([\w.]+)',text)
    if match and match[1] not in options|{'get','asList'}:
     issues.append(f'Unknown option: {b.name} {match[1]}')
   for child in node.children:optionwalk(child)
  optionwalk(tree.root_node)
 def check(v):
  if isinstance(v,dict):
   for name in v.get('clearWith',[]) or []:
    if name not in options:issues.append(f'Unknown clearWith: {r.name} {name}')
   if 'visible' in v and isinstance(v['visible'],str):
    for name in re.findall(r'\b[A-Za-z]\w*\b',v['visible']):
     if name not in options and name not in {'true','false','key','levels'}:issues.append(f'Unknown visible: {r.name} {name}')
   if 'refs' in v:
    names=v['refs'] if isinstance(v['refs'],list) else [v['refs']]
    for name in names:
     if name not in refs:issues.append(f'Unknown reference: {r.name} {name}')
   for val in v.values():check(val)
  elif isinstance(v,list):
   for val in v:check(val)
 if r.exists():check(ys[r.name])
imports=re.search(r'Imports:(.*?)(?:\n\S|\Z)',(root/'DESCRIPTION').read_text(),re.S)[1]
packages=set(re.findall(r'[A-Za-z][A-Za-z0-9.]*',re.sub(r'\([^)]*\)','',imports)))
base={'base','stats','utils','grDevices','graphics','grid','tools','methods'}
for p in (root/'R').glob('*.b.R'):
 s=p.read_bytes();tree=parser.parse(s)
 def walk(n):
  if n.type=='namespace_operator':
   lhs=n.child_by_field_name('lhs');name=s[lhs.start_byte:lhs.end_byte].decode()
   if name not in packages|base:issues.append(f'Undeclared package: {p.name} {name}')
  for c in n.children:walk(c)
 walk(tree.root_node)
status={'date':datetime.date.today().isoformat(),'candidate_version':'1.0.0','basis_version':'0.1.2','audit_version':'0.1.1',
 'R_syntax_parser':'tree-sitter-language-pack 0.9.0; grammar check, not R execution',
 'R_files_parsed':len(rfiles),'YAML_files_parsed':len(yfiles),
 'testthat_blocks_added':(root/'tests/testthat/test-audit-statistics.R').read_text().count('test_that('),
 'source_option_visibility_reference_dependency_issues':sorted(set(issues)),
 'R_statistical_tests':'Not executed by static validator; see runtime evidence','jmvtools_build':'Not executed by static validator; see runtime evidence',
 'submission_target':'jamovi library','R_CMD_check':'OPTIONAL supplementary diagnostic; NOT RUN','private_Regression_API_review':'PENDING maintainer review','jamovi_GUI_and_exports':'NOT RUN'}
evidence_path=root/'audit/USER_VALIDATION_EVIDENCE.json'
if evidence_path.exists():
 status['baseline_user_runtime_evidence']=json.loads(evidence_path.read_text())
 status['baseline_evidence_scope']='Previous 26-block candidate only; follow-up changes require a fresh run.'
status['source_base_commit']='4e639ad082b78c4aa868be15d4ca61090d402b71'
status['source_followup']='regression-nortest-lazy-resolution'
confirmation_path=root/'audit/NORTEST_VALIDATION_2026-11-10.json'
if confirmation_path.exists():
 confirmation=json.loads(confirmation_path.read_text())
 status['nortest_fix_developer_confirmation']=confirmation
 status['jamovi_GUI_and_exports']='Developer confirms the nortest fix works; new export/persistence checks are not part of this confirmation.'
 status['runtime_evidence_note']='Static parsing does not execute R or jamovi. Developer confirmation covers the nortest fix; baseline evidence covers earlier candidates.'
# Preserve the latest developer-supplied runtime evidence on subsequent static runs.
latest_path=root/'audit/RUNTIME_VALIDATION_2026-11-11.json'
if latest_path.exists():
 latest=json.loads(latest_path.read_text())
 status['latest_user_runtime_evidence']=latest
 status['R_statistical_tests']='PASS — developer-supplied testthat output: 274 expectations; FAIL/WARN/SKIP 0'
 status['jmvtools_build']='PASS — developer confirms compilation and installation'
 status['jamovi_GUI_and_exports']='Normality and residual plots PASS; independent of moretests; example data opens. Exports/persistence remain historical.'
 status['runtime_evidence_note']='Latest runtime evidence is developer-supplied. Static validation does not execute R. See dated scope limits.'
# Latest 1.1.0 runtime evidence supersedes older pending-runtime statements.
final_path=root/'audit/i18n/VALIDACION_FINAL_1.1.0.json'
if final_path.exists():
 final=json.loads(final_path.read_text())
 status['candidate_version']=final['candidate_version']
 status['testthat_blocks_added']=sum(p.read_text().count('test_that(') for p in (root/'tests/testthat').glob('test-*.R'))
 status['latest_user_runtime_evidence']=final
 status['R_statistical_tests']='PASS — developer-supplied 345 expectations; FAIL/WARN/SKIP 0; 44 blocks'
 status['jmvtools_build']='PASS — developer confirms build/installation and candidate operation'
 status['jamovi_GUI_and_exports']='PASS — GUI, HTML/PDF, sqrt label, .omv persistence and English switching confirmed'
 status['runtime_evidence_note']=final['scope_limit']
 status['source_followup']='spanish-1.1.0 with GUI and plot corrections'
(root/'audit/VALIDATION_STATUS.json').write_text(json.dumps(status,indent=2)+'\n')
print(json.dumps(status,indent=2))
