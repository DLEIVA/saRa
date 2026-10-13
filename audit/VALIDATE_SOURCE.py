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
 'R_statistical_tests':'NOT RUN: R executable unavailable','jmvtools_build':'NOT RUN',
 'submission_target':'jamovi library','R_CMD_check':'OPTIONAL supplementary diagnostic; NOT RUN','private_Regression_API_review':'PENDING maintainer review','jamovi_GUI_and_exports':'NOT RUN'}
evidence_path=root/'audit/USER_VALIDATION_EVIDENCE.json'
if evidence_path.exists():
 status['baseline_user_runtime_evidence']=json.loads(evidence_path.read_text())
 status['baseline_evidence_scope']='Previous 26-block candidate only; follow-up changes require a fresh run.'
status['source_base_commit']='8f54e75988409bb033cdbfe18365c8c07a6da8af'
status['source_followup']='association-helpers-and-shared-routines'
(root/'audit/VALIDATION_STATUS.json').write_text(json.dumps(status,indent=2)+'\n')
print(json.dumps(status,indent=2))
