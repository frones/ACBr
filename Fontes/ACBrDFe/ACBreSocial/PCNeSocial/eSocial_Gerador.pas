{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 29/02/2016: Guilherme Costa
|*  - Atribuindo o namespace ao URI para validação dos XSD
|* 01/03/2016: Guilherme Costa
|*  - Alterações para validação com os XSD
|* 02/03/2016: Robson Batista Neto
|*  - Alteração nas linhas 271 e 274 Não estava gravando o nome do xml
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}
{$I ACBr.inc}

unit eSocial_Gerador;

interface

uses
  SysUtils, Classes, StrUtils, variants,
  ACBrDFEUtil, ACBrUtil,
  pcnGerador, pcnConversao, pcnAuxiliar,
  eSocial_Common, eSocial_Conversao;

type
  TGeradorOpcoes = class;
  TeSocialEvento = class;

  TeSocialEvento = class(TeSocial)
  private
    FXMLAssinado: String;
    FXMLOriginal: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FAlertas: String;
    FXML: AnsiString;
    FGerador: TGerador;
    FSchema: TeSocialSchema;
    FACBreSocial : TObject;//alimenta no create
  public
    constructor Create(AACBreSocial: TObject); overload;//->recebe a instancia da classe TACBreSocial
    destructor Destroy; override;
    function  GerarXML: boolean; virtual; abstract;
    procedure SaveToFile(const CaminhoArquivo: string);
    function  Assinar(XMLEvento: String; NomeEvento: String): AnsiString;
    function  GerarChaveEsocial(const emissao: TDateTime; const CNPJF: string; sequencial: Integer; AOrgaoPublico: Boolean = False): String;
    procedure Validar(Evento: String);

    property Alertas: String read FAlertas;
    property ErroValidacao: String read FErroValidacao;
    property ErroValidacaoCompleto: String read FErroValidacaoCompleto;
  protected
    {Geradores de Uso Comum}
    procedure GerarCabecalho(Namespace: String);
    procedure GerarRodape;
    procedure GerarAliqGilRat( pAliqRat: TAliqGilRat;const GroupName: string = 'aliqGilRat');
    procedure GerarAlvaraJudicial(pAlvaraJudicial: TAlvaraJudicial);
    procedure GerarAposentadoria(pAposentadoria: TAposentadoria);
    procedure GerarCNH(pCnh: TCNH);
    procedure GerarContatoTrabalhador(pContato: TContatoTrabalhador);
    procedure GerarInfoContrato(pInfoContrato: TInfoContrato);
    procedure GerarCTPS(pCTPS: TCTPS);
    procedure GerarDependente(pDependente: TDependenteCollection);
    procedure GerarDescAtividade(pDescAtividade: TDescAtividadeCollection);
    procedure GerarDocumentos(pDocumentos: TDocumentos);
    procedure GerarDuracao(pDuracao: TDuracao);
    procedure GerarEndereco(pEndereco: TEndereco; pExterior: boolean = false);
    procedure GerarEnderecoBrasil(pEndereco: TBrasil);
    procedure GerarEnderecoExterior(pEndereco: TExterior);
    procedure GerarEpi(pEpi: TEpiCollection);
    procedure GerarFGTS(pFgts: TFGTS);
    procedure GerarFiliacaoSindical(pFiliacaoSindical: TFiliacaoSindical);
    procedure GerarHorario(pHorario: THorarioCollection);
    procedure GerarHorContratual(pHorContratual: THorContratual);
    procedure GerarIdeEvento(pEvt: TIdeEvento; const GeraGrupo: boolean = True);
    procedure GerarIdeEvento2(pEvt: TIdeEvento2; const GeraGrupo: boolean = True; GeraRetif: Boolean = True);
    procedure GerarIdeEvento3(pEvt: TIdeEvento3; IndRetif: Boolean=True);
    procedure GerarIdeEvento4(pEvt: TIdeEvento4);
    procedure GerarIdePeriodo(pIdePeriodo: TidePeriodo; const GroupName: string = 'idePeriodo');
    procedure GerarIdeEmpregador(pEmp: TIdeEmpregador; pNumTags: integer = 2);
    procedure GerarIdeTomadorServ(pIdeTomadorServ: TIdeTomadorServ);
    procedure GerarIdeEstabVinc(pIdeEstabVinc: TIdeEstabVinc);
    procedure GerarIdeTrabSubstituido(pIdeTrabSubstituido: TIdeTrabSubstituidoCollection);
    procedure GerarIdVersao(pIdEsocial: TeSocial);
    procedure GerarIdeVinculo(pIdeVinculo: TIdeVinculo);
    procedure GerarInfoAtivDesemp(pInfoAtivDesemp: TInfoAtivDesemp);
    procedure GerarInfoDeficiencia(pInfoDeficiencia: TInfoDeficiencia; pTipo: integer = 0);
    procedure GerarLocalTrabGeral(pLocalTrabGeral: TLocalTrabGeral);
    procedure GerarLocalTrabDom(pLocalTrabDom: TLocalTrabDom);
    procedure GerarLocalTrabalho(pLocalTrabalho: TLocalTrabalho);
    procedure GerarModoAbertura(pModo: TModoLancamento);
    procedure GerarModoFechamento(pModo: TModoLancamento);
    procedure GerarNascimento(pNasc: TNascimento; pGroupName: string = 'nascimento');
    procedure GerarProcessoGenerico(pChave: string; pProcesso: TProcesso);
    procedure GerarProcessoAdmJudFap(pProcAdmJudFap: TProcAdmJudFap);
    procedure GerarProcessoAdmJudRat(pProcAdmJudRat: TProcAdmJudRat);
    procedure GerarRemuneracao(pRemuneracao: TRemuneracao);
    procedure GerarRG(pRg: TRg);
    procedure GerarRNE(pRNE: TRNE);
    procedure GerarRic(pRic: TRic);
    procedure GerarOC(pOc: TOC);
    procedure GerarSucessaoVinc(pSucessaoVinc: TSucessaoVinc);
    procedure GerarTrabalhador(pTrabalhador: TTrabalhador; const GroupName: string = 'trabalhador'; const Tipo: Integer = 1);
    procedure GerarTrabEstrangeiro(pTrabEstrangeiro: TTrabEstrangeiro);
    procedure GerarTrabTemporario(pTrabTemporario: TTrabTemporario);
    procedure GerarInfoASO(pInfoASO: TInfoASO);
    procedure GerarAfastamento(pAfastamento: TAfastamento);
    procedure GerarDesligamento(pDesligamento: TDesligamento);
    procedure GerarVinculo(pVinculo: TVinculo; pTipo: integer = 1);
    procedure GerarInfoRegimeTrab(pInfoRegimeTrab: TInfoRegimeTrab);
    procedure GerarInfoAprend(pAprend: TAprend);
    procedure GerarInfoCeletista(pInfoCeletista: TInfoCeletista);
    procedure GerarInfoDecJud(pInfoDecJud: TInfoDecJud);
    procedure GerarInfoEstatutario(pInfoEstatutario: TInfoEstatutario);
    procedure gerarIdeTrabalhador(
      pideTrabalhador: TideTrabalhador;
      const GeraGrupo: boolean = True
    );
    procedure gerarIdeTrabalhador2(
      pideTrabalhador: TideTrabalhador2;
      const GeraGrupo: boolean
    );
    procedure gerarIdeFolhaPagto(pIdeFolhaPagto: TIdeFolhaPagto);
    procedure gerarEmitente(pEmitente: TEmitente);
    procedure GerarEndExt(pEndExt: TEndExt);
    procedure GerarIdePais(pIdePais: TIdePais);
    procedure GerarInfoAgNocivo(pInfoAgNocivo: TInfoAgNocivo);
    procedure GerarItensRemun(
      objItensRemun: TRubricaCollection;
      const GroupName: string = 'verbasResc'
    );
    procedure GerarProcJudTrab(objProcJudTrab: TProcJudTrabCollection);
    procedure GerarPensaoAlim(objPensaoAlim: TPensaoAlimCollection; const GroupName: String = 'pensaoAlim');
    procedure GerarInfoSaudeColet(objInfoSaudeColet: TInfoSaudeColet);
    procedure GerarDetPlano(objDetPlanoCollection: TDetPlanoCollection);
    procedure GerarDetOper(objDetOper: TDetOperCollection);
    procedure GerarNfs(pNfs: TNfsColecao);
    procedure GerarRemunOutrEmpr(objRemunOutrEmpr: TRemunOutrEmprCollection);
    procedure GerarInfoMV(pInfoMV: TInfoMV);
    procedure GerarInfoSimples(obj: TinfoSimples);
    procedure GerarIdeEstabLot(pIdeEstabLot : TideEstabLotCollection);
    procedure GerarQuarentena(obj: TQuarentena);
    procedure GerarIdeRespInf(obj: TIdeRespInf);
  published
    property Gerador: TGerador  read FGerador write FGerador;
    property schema: TeSocialSchema read Fschema write Fschema;
    property XML: AnsiString read FXML write FXML;
  end;

  TGeradorOpcoes = class(TPersistent)
  private
    FAjustarTagNro: boolean;
    FGerarTagIPIparaNaoTributado: boolean;
    FGerarTXTSimultaneamente: boolean;
    FNormatizarMunicipios: boolean;
    FGerarTagAssinatura: TpcnTagAssinatura;
    FPathArquivoMunicipios: string;
    FValidarInscricoes: boolean;
    FValidarListaServicos: boolean;
  published
    property AjustarTagNro: boolean read FAjustarTagNro write FAjustarTagNro;
    property GerarTagIPIparaNaoTributado: boolean read FGerarTagIPIparaNaoTributado write FGerarTagIPIparaNaoTributado;
    property GerarTXTSimultaneamente: boolean read FGerarTXTSimultaneamente write FGerarTXTSimultaneamente;
    property NormatizarMunicipios: boolean read FNormatizarMunicipios write FNormatizarMunicipios;
    property GerarTagAssinatura: TpcnTagAssinatura read FGerarTagAssinatura write FGerarTagAssinatura;
    property PathArquivoMunicipios: string read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: boolean read FValidarInscricoes write FValidarInscricoes;
    property ValidarListaServicos: boolean read FValidarListaServicos write FValidarListaServicos;
  end;

implementation

uses
  ACBreSocial, pcnLeitor;

{TeSocialEvento}

function TeSocialEvento.Assinar(XMLEvento, NomeEvento: String): AnsiString;
var
  XMLAss: String;
  ArqXML: String;
  Leitor: TLeitor;
//  i: Integer;
begin
  Result := '';

  ArqXML := XMLEvento;

  // XML jÃ¡ deve estar em UTF8, para poder ser assinado //
  ArqXML := ConverteXMLtoUTF8(ArqXML);
  FXMLOriginal := ArqXML;

  with TACBreSocial(FACBreSocial) do
  begin
    XMLAss := SSL.Assinar(ArqXML, 'eSocial', NomeEvento, '', '', 'id');
    FXMLAssinado := XMLAss;
    FXMLOriginal := XMLAss;

    XMLAss := StringReplace(XMLAss, '<' + ENCODING_UTF8_STD + '>', '', [rfReplaceAll]);
    XMLAss := StringReplace(XMLAss, '<' + XML_V01 + '>', '', [rfReplaceAll]);

    NomeEvento := NomeEvento + '.xml';

    if Configuracoes.Arquivos.Salvar then
       Gravar(NomeEvento, XMLAss,Configuracoes.Arquivos.PathSalvar);

    Result := XMLAss;

    {$IFDEF DEBUG}
      With TStringList.Create do
      try
        Text := XMLAss;
        SaveToFile(IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'Docs\' + NomeEvento);
      finally
        Free;
      end;
    {$ENDIF}
  end;
end;
constructor TeSocialEvento.Create(AACBreSocial: TObject);
begin
  FACBreSocial := AACBreSocial;
  FGerador := TGerador.Create;
  FGerador.ArquivoFormatoXML := '';
end;

destructor TeSocialEvento.Destroy;
begin
  FGerador.Free;
end;

procedure TeSocialEvento.SaveToFile(const CaminhoArquivo: string);
var
  lStr: TStringList;
  lFileName: string;
begin
  lFileName := CaminhoArquivo;
  lStr:= TStringList.Create;
  try
    lStr.Text := XML;
    lStr.SaveToFile(ChangeFileExt(lFileName,'.xml'));
  finally
    lStr.Free;
  end;
end;

procedure TeSocialEvento.Validar(Evento: String);
var
  Erro, AXML: String;
  EhValido: Boolean;
begin
  AXML := FXMLAssinado;

  if EstaVazio(AXML) then
  begin
    Assinar(AXML, Evento);
    AXML := FXMLAssinado;
  end;

  with TACBreSocial(FACBreSocial) do
  begin
    EhValido := SSL.Validar(AXML, Configuracoes.Arquivos.PathSchemas+Evento+'.xsd', Erro);
    if not EhValido then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados do evento: ') +
        Evento + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      {$IFDEF DEBUG}
      with TStringList.Create do
      try
        Add(AXML);
        Add('<!--' + FErroValidacaoCompleto + '-->');
        SaveToFile(Configuracoes.Arquivos.PathSalvar+Evento+'_error' +'.xml');
      finally
        Free;
      end;
      {$ENDIF}
      raise EACBreSocialException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

procedure TeSocialEvento.GerarCabecalho(Namespace: String);
begin
  // TODO: layout antigo remover na versão do 2.3
  TACBreSocial(FACBreSocial).SSL.NameSpaceURI := 'http://www.esocial.gov.br/schema/evt/'+Namespace+'/v02_02_02'; // << /v02_03_00
  Gerador.wGrupo(ENCODING_UTF8, '', False);
  Gerador.wGrupo('eSocial xmlns="'+TACBreSocial(FACBreSocial).SSL.NameSpaceURI+'"');
end;

function TeSocialEvento.GerarChaveEsocial(const emissao: TDateTime; const CNPJF: string; sequencial: Integer; AOrgaoPublico: Boolean): String;
var
  nAno, nMes, nDia, nHora, nMin, nSeg, nMSeg: Word;
begin
  // Se o usuario informar 0; o código numerico sera gerado de maneira aleatória //
  if sequencial = 0 then
    sequencial := Random(99999);
  DecodeDate(emissao, nAno, nMes, nDia);
  DecodeTime(emissao, nHora, nMin, nSeg, nMSeg);
  Result := 'ID';

  if (Length(CNPJF) = 14) then
    Result := Result + IntToStr(1)
  else
    Result := Result + IntToStr(2);

  if AOrgaoPublico then
    Result := Result + copy(SomenteNumeros(CNPJF) + '00000000000000', 1, 14)
  else
    Result := Result + copy(SomenteNumeros(Copy(CNPJF, 1, 8)) + '00000000000000', 1, 14);
  Result := Result + IntToStrZero(nAno, 4);
  Result := Result + IntToStrZero(nMes, 2);
  Result := Result + IntToStrZero(nDia, 2);
  Result := Result + IntToStrZero(nHora, 2);
  Result := Result + IntToStrZero(nMin, 2);
  Result := Result + IntToStrZero(nSeg, 2);
  Result := Result + IntToStrZero(sequencial, 5);
end;

procedure TeSocialEvento.GerarCNH(pCnh: TCNH);
begin
  if pCnh.nrRegCnh <> EmptyStr then
  begin
    Gerador.wGrupo('CNH');
      Gerador.wCampo(tcStr, '', 'nrRegCnh  ', 0, 0, 0, pCnh.nrRegCnh);
      Gerador.wCampo(tcDat, '', 'dtExped  ', 0, 0, 0, pCnh.DtExped);
      Gerador.wCampo(tcStr, '', 'ufCnh  ', 0, 0, 0, eSufToStr(pCnh.ufCnh));
      Gerador.wCampo(tcDat, '', 'dtValid  ', 0, 0, 0, pCnh.DtValid);

      if (DateToStr(pCnh.dtPriHab) <> '') then
        Gerador.wCampo(tcDat, '', 'dtPriHab  ', 0, 0, 0, pCnh.dtPriHab);
      Gerador.wCampo(tcStr, '', 'categoriaCnh  ', 0, 0, 0, eSCnhToStr(pCnh.categoriaCnh));
    Gerador.wGrupo('/CNH');
  end;
end;

procedure TeSocialEvento.GerarContatoTrabalhador(pContato: TContatoTrabalhador);
begin
  Gerador.wGrupo('contato');
    if (pContato.FonePrinc <> '') then
      Gerador.wCampo(tcStr, '', 'fonePrinc  ', 0, 0, 0, pContato.FonePrinc);

    if (pContato.FoneAlternat <> '') then
      Gerador.wCampo(tcStr, '', 'foneAlternat  ', 0, 0, 0, pContato.FoneAlternat);

    if (pContato.emailPrinc <> '') then
      Gerador.wCampo(tcStr, '', 'emailPrinc  ', 0, 0, 0, pContato.emailPrinc);

    if (pContato.EmailAlternat <> '') then
      Gerador.wCampo(tcStr, '', 'emailAlternat  ', 0, 0, 0, pContato.EmailAlternat);
  Gerador.wGrupo('/contato');
end;

procedure TeSocialEvento.GerarInfoContrato(pInfoContrato: TInfoContrato);
begin
  Gerador.wGrupo('infoContrato');
    if (pInfoContrato.CodCargo <> '')  then
      Gerador.wCampo(tcStr, '', 'codCargo  ', 0, 0, 0, pInfoContrato.CodCargo);

    if (pInfoContrato.CodFuncao <> '') then
      Gerador.wCampo(tcStr, '', 'codFuncao  ', 0, 0, 0, pInfoContrato.CodFuncao);

    Gerador.wCampo(tcStr, '', 'codCateg  ', 0, 0, 0, pInfoContrato.CodCateg);

    if (pInfoContrato.codCarreira <> '') then
    begin
      Gerador.wCampo(tcStr, '', 'codCarreira  ', 0, 0, 0, pInfoContrato.codCarreira);
      Gerador.wCampo(tcDat, '', 'dtIngrCarr  ', 0, 0, 0, pInfoContrato.dtIngrCarr);
    end;
    GerarRemuneracao(pInfoContrato.Remuneracao);
    GerarDuracao(pInfoContrato.Duracao);
    GerarLocalTrabalho(pInfoContrato.LocalTrabalho);
    GerarHorContratual(pInfoContrato.HorContratual);
    GerarFiliacaoSindical(pInfoContrato.FiliacaoSindical);
    //GerarInfoAtivDesemp(pInfoContrato.InfoAtivDesemp);
    GerarAlvaraJudicial(pInfoContrato.AlvaraJudicial);
  Gerador.wGrupo('/infoContrato');
end;


procedure TeSocialEvento.GerarCTPS(pCTPS: TCTPS);
begin
  Gerador.wGrupo('CTPS');
    Gerador.wCampo(tcStr, '', 'nrCtps  ', 0, 0, 0, pCTPS.NrCtps);
    Gerador.wCampo(tcStr, '', 'serieCtps  ', 0, 0, 0, pCTPS.SerieCtps);
    Gerador.wCampo(tcStr, '', 'ufCtps  ', 0, 0, 0, pCTPS.UfCtps);
  Gerador.wGrupo('/CTPS');
end;

procedure TeSocialEvento.GerarDependente(pDependente: TDependenteCollection);
var
  i: integer;
begin
  for i:= 0 to pDependente.Count-1 do
  begin
    Gerador.wGrupo('dependente');
      Gerador.wCampo(tcStr, '', 'tpDep  ', 0, 0, 0, eStpDepToStr(pDependente.Items[i].TpDep));
      Gerador.wCampo(tcStr, '', 'nmDep  ', 0, 0, 0, pDependente.Items[i].NmDep);
      Gerador.wCampo(tcDat, '', 'dtNascto  ', 0, 0, 0, pDependente.Items[i].DtNascto);

      if (pDependente.Items[i].CpfDep <> '') then
        Gerador.wCampo(tcStr, '', 'cpfDep  ', 0, 0, 0, pDependente.Items[i].CpfDep);

      Gerador.wCampo(tcStr, '', 'depIRRF  ', 0, 0, 0, eSSimNaoToStr(pDependente.Items[i].DepIRRF));
      Gerador.wCampo(tcStr, '', 'depSF  ', 0, 0, 0, eSSimNaoToStr(pDependente.Items[i].DepSF));
      Gerador.wCampo(tcStr, '', 'depPlan  ', 0, 0, 0, eSSimNaoToStr(pDependente.Items[i].depPlan));
      Gerador.wCampo(tcStr, '', 'incTrab  ', 0, 0, 0, eSSimNaoToStr(pDependente.Items[i].incTrab));
    Gerador.wGrupo('/dependente');
  end;
end;

procedure TeSocialEvento.GerarDescAtividade(pDescAtividade: TDescAtividadeCollection);
var
  i: integer;
begin
  for i:= 0 to pDescAtividade.Count-1 do
  begin
    Gerador.wGrupo('descAtividade');
      Gerador.wCampo(tcStr, '', 'descAtivDesemp  ', 0, 0, 0, pDescAtividade.Items[i].DescAtivDesemp);
    Gerador.wGrupo('/descAtividade');
  end;
end;

procedure TeSocialEvento.GerarDesligamento(pDesligamento: TDesligamento);
begin
  if pDesligamento.DtDeslig > 0 then
  begin
    Gerador.wGrupo('desligamento');
      Gerador.wCampo(tcDat, '', 'dtDeslig', 0, 0, 0, pDesligamento.DtDeslig);
    Gerador.wGrupo('/desligamento');
  end;
end;

procedure TeSocialEvento.GerarDocumentos(pDocumentos: TDocumentos);
begin
  Gerador.wGrupo('documentos');
    GerarCTPS(pDocumentos.CTPS);
    GerarRic(pDocumentos.Ric);
    GerarRG(pDocumentos.RG);
    GerarRNE(pDocumentos.RNE);
    GerarOC(pDocumentos.OC);
    GerarCNH(pDocumentos.CNH);
  Gerador.wGrupo('/documentos');
end;

procedure TeSocialEvento.GerarDuracao(pDuracao: TDuracao);
begin
  Gerador.wGrupo('duracao');
    Gerador.wCampo(tcStr, '', 'tpContr  ', 0, 0, 0, eSTpContrToStr(pDuracao.TpContr));

    if (eSTpContrToStr(pDuracao.TpContr) = '2') then
      Gerador.wCampo(tcDat, '', 'dtTerm  ', 0, 0, 0, pDuracao.dtTerm);
  Gerador.wGrupo('/duracao');
end;

procedure TeSocialEvento.GerarEndereco(pEndereco: TEndereco;
  pExterior: boolean);
begin
  Gerador.wGrupo('endereco');
  if not pExterior then
    GerarEnderecoBrasil(pEndereco.Brasil)
  else
    GerarEnderecoExterior(pEndereco.Exterior);

  Gerador.wGrupo('/endereco');
end;

procedure TeSocialEvento.GerarEnderecoBrasil(pEndereco: TBrasil);
begin
  Gerador.wGrupo('brasil');
    Gerador.wCampo(tcStr, '', 'tpLograd  ', 0, 0, 0, pEndereco.TpLograd);
    Gerador.wCampo(tcStr, '', 'dscLograd  ', 0, 0, 0, pEndereco.DscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd  ', 0, 0, 0, pEndereco.NrLograd);

    if (pEndereco.Complemento <> '') then
      Gerador.wCampo(tcStr, '', 'complemento  ', 0, 0, 0, pEndereco.Complemento);

    if (pEndereco.Bairro <> '') then
      Gerador.wCampo(tcStr, '', 'bairro  ', 0, 0, 0, pEndereco.Bairro);

    Gerador.wCampo(tcStr, '', 'cep  ', 0, 0, 0, pEndereco.Cep);
    Gerador.wCampo(tcStr, '', 'codMunic  ', 0, 0, 0, pEndereco.CodMunic);
    Gerador.wCampo(tcStr, '', 'uf  ', 0, 0, 0, eSufToStr(pEndereco.UF));
  Gerador.wGrupo('/brasil');
end;

procedure TeSocialEvento.GerarEnderecoExterior(pEndereco: TExterior);
begin
  Gerador.wGrupo('exterior');
    Gerador.wCampo(tcStr, '', 'paisResid  ', 0, 0, 0, pEndereco.PaisResid);
    Gerador.wCampo(tcStr, '', 'dscLograd  ', 0, 0, 0, pEndereco.DscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd  ', 0, 0, 0, pEndereco.NrLograd);

    if (pEndereco.Complemento <> '') then
      Gerador.wCampo(tcStr, '', 'complemento  ', 0, 0, 0, pEndereco.Complemento);

    if (pEndereco.Bairro <> '') then
      Gerador.wCampo(tcStr, '', 'bairro  ', 0, 0, 0, pEndereco.Bairro);

    Gerador.wCampo(tcStr, '', 'nmCid  ', 0, 0, 0, pEndereco.NmCid);

    if (pEndereco.CodPostal <> '') then
      Gerador.wCampo(tcStr, '', 'codPostal  ', 0, 0, 0, pEndereco.CodPostal);
  Gerador.wGrupo('/exterior');
end;

procedure TeSocialEvento.GerarEndExt(pEndExt: TEndExt);
begin
  Gerador.wGrupo('endExt');
    Gerador.wCampo(tcStr, '', 'dscLograd', 0, 0, 0, pEndExt.dscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd', 0, 0, 0, pEndExt.nrLograd);
    Gerador.wCampo(tcStr, '', 'complem', 0, 0, 0, pEndExt.complem);
    Gerador.wCampo(tcStr, '', 'bairro', 0, 0, 0, pEndExt.bairro);
    Gerador.wCampo(tcStr, '', 'nmCid', 0, 0, 0, pEndExt.nmCid);
    Gerador.wCampo(tcStr, '', 'codPostal', 0, 0, 0, pEndExt.codPostal);
  Gerador.wGrupo('/endExt');
end;

procedure TeSocialEvento.GerarEpi(pEpi: TEpiCollection);
var
  i: integer;
begin
  for i := 0 to pEpi.Count - 1 do
  begin
    Gerador.wGrupo('epi');
      Gerador.wCampo(tcStr, '', 'caEPI', 0, 0, 0,  pEpi.Items[i].caEPI);
    Gerador.wGrupo('/epi');
  end;
end;

procedure TeSocialEvento.GerarFGTS(pFgts: TFGTS);
begin
  Gerador.wGrupo('FGTS');
    Gerador.wCampo(tcStr, '', 'opcFGTS  ', 0, 0, 0, eSOpcFGTSToStr(pFGTS.OpcFGTS));

    if (eSOpcFGTSToStr(pFGTS.OpcFGTS) = '1') then
      Gerador.wCampo(tcDat, '', 'dtOpcFGTS  ', 0, 0, 0, pFgts.DtOpcFGTS);
  Gerador.wGrupo('/FGTS');
end;

procedure TeSocialEvento.GerarFiliacaoSindical(
  pFiliacaoSindical: TFiliacaoSindical);
var
  i: Integer;
begin
  for i := 0 to pFiliacaoSindical.Count - 1 do
  begin
    Gerador.wGrupo('filiacaoSindical');
      Gerador.wCampo(tcStr, '', 'cnpjSindTrab  ', 0, 0, 0, pFiliacaoSindical[i].CnpjSindTrab);
    Gerador.wGrupo('/filiacaoSindical');
  end;
end;

procedure TeSocialEvento.GerarHorario(pHorario: THorarioCollection);
var
  i: integer;
begin
  for i:= 0 to pHorario.Count-1 do
  begin
    Gerador.wGrupo('horario');
      Gerador.wCampo(tcStr, '', 'dia', 0, 0, 0, eSTpDiaToStr(pHorario.Items[i].Dia));
      Gerador.wCampo(tcStr, '', 'codHorContrat  ', 0, 0, 0, pHorario.Items[i].CodHorContrat);
    Gerador.wGrupo('/horario');
  end;
end;

procedure TeSocialEvento.GerarHorContratual(pHorContratual: THorContratual);
begin
  Gerador.wGrupo('horContratual');
    Gerador.wCampo(tcStr, '', 'qtdHrsSem  ', 0, 0, 0, pHorContratual.QtdHrsSem);
    Gerador.wCampo(tcStr, '', 'tpJornada  ', 0, 0, 0, eSTpJornadaToStr(pHorContratual.TpJornada));

    if (eSTpJornadaToStr(pHorContratual.TpJornada) = '2') then
      Gerador.wCampo(tcStr, '', 'dscTpJorn  ', 0, 0, 0, pHorContratual.DscTpJorn);
    Gerador.wCampo(tcStr, '', 'tmpParc  ', 0, 0, 0, eSSimNaoToStr(pHorContratual.tmpParc));
    GerarHorario(pHorContratual.horario);
  Gerador.wGrupo('/horContratual');
end;

procedure TeSocialEvento.GerarRemuneracao(pRemuneracao: TRemuneracao);
begin
  if pRemuneracao.VrSalFx > 0 then
  begin
    Gerador.wGrupo('remuneracao');
      Gerador.wCampo(tcStr, '', 'vrSalFx  ', 0, 0, 0, pRemuneracao.VrSalFx);
      Gerador.wCampo(tcStr, '', 'undSalFixo  ', 0, 0, 0, eSUndSalFixoToStr(pRemuneracao.UndSalFixo));

      if (eSUndSalFixoToStr(pRemuneracao.UndSalFixo) = '7') then
        Gerador.wCampo(tcStr, '', 'dscSalVar  ', 0, 0, 0, pRemuneracao.DscSalVar);
    Gerador.wGrupo('/remuneracao');
  end;
end;

procedure TeSocialEvento.GerarRG(pRg: TRg);
begin
  if pRg.NrRg <> EmptyStr then
  begin
    Gerador.wGrupo('RG');
      Gerador.wCampo(tcStr, '', 'nrRg  ', 0, 0, 0, pRg.NrRg );
      Gerador.wCampo(tcStr, '', 'orgaoEmissor  ', 0, 0, 0, pRg.OrgaoEmissor);
      Gerador.wCampo(tcDat, '', 'dtExped  ', 0, 0, 0, pRg.DtExped);
    Gerador.wGrupo('/RG');
  end;
end;

procedure TeSocialEvento.GerarRic(pRic: TRic);
begin
  if pRic.NrRic <> EmptyStr then
  begin
    Gerador.wGrupo('RIC');
      Gerador.wCampo(tcStr, '', 'nrRic  ', 0, 0, 0, pRic.NrRic );
      Gerador.wCampo(tcStr, '', 'orgaoEmissor  ', 0, 0, 0, pRic.OrgaoEmissor);
      Gerador.wCampo(tcDat, '', 'dtExped  ', 0, 0, 0, pRic.DtExped);
    Gerador.wGrupo('/RIC');
  end;
end;

procedure TeSocialEvento.GerarRNE(pRNE: TRNE);
begin
  if pRNE.NrRne  <> EmptyStr then
  begin
  Gerador.wGrupo('RNE');
    Gerador.wCampo(tcStr, '', 'nrRne  ', 0, 0, 0, pRNE.NrRne );
    Gerador.wCampo(tcStr, '', 'orgaoEmissor  ', 0, 0, 0, pRNE.OrgaoEmissor);
    Gerador.wCampo(tcDat, '', 'dtExped ', 0, 0, 0, pRNE.DtExped);
  Gerador.wGrupo('/RNE');
  end;
end;

procedure TeSocialEvento.GerarRodape;
begin
  Gerador.wGrupo('/eSocial');
end;

procedure TeSocialEvento.GerarSucessaoVinc(pSucessaoVinc: TSucessaoVinc);
begin
  if pSucessaoVinc.cnpjEmpregAnt <> EmptyStr then
  begin
  Gerador.wGrupo('sucessaoVinc');
    Gerador.wCampo(tcStr, '', 'cnpjEmpregAnt  ', 0, 0, 0, pSucessaoVinc.cnpjEmpregAnt);
    Gerador.wCampo(tcStr, '', 'matricAnt  ', 0, 0, 0, pSucessaoVinc.MatricAnt);
    Gerador.wCampo(tcDat, '', 'dtIniVinculo  ', 0, 0, 0, pSucessaoVinc.DtIniVinculo);

    if (pSucessaoVinc.Observacao <> '') then
      Gerador.wCampo(tcStr, '', 'observacao  ', 0, 0, 0, pSucessaoVinc.Observacao);

  Gerador.wGrupo('/sucessaoVinc');
  end;
end;

procedure TeSocialEvento.GerarTrabalhador(pTrabalhador: TTrabalhador; const GroupName: string;const tipo: Integer);
begin
  Gerador.wGrupo(GroupName);
    if (GroupName = 'trabalhador') then
      Gerador.wCampo(tcStr, '', 'cpfTrab  ', 0, 0, 0, pTrabalhador.CpfTrab);

    Gerador.wCampo(tcStr, '', 'nisTrab  ', 0, 0, 0, pTrabalhador.NisTrab);
    Gerador.wCampo(tcStr, '', 'nmTrab  ', 0, 0, 0, pTrabalhador.NmTrab);
    Gerador.wCampo(tcStr, '', 'sexo  ', 0, 0, 0, pTrabalhador.Sexo);
    Gerador.wCampo(tcStr, '', 'racaCor  ', 0, 0, 0, pTrabalhador.RacaCor);

    if ((pTrabalhador.EstCiv >= 1) and (pTrabalhador.EstCiv <= 5)) then
      Gerador.wCampo(tcStr, '', 'estCiv  ', 0, 0, 0, pTrabalhador.EstCiv);

    Gerador.wCampo(tcStr, '', 'grauInstr  ', 0, 0, 0, pTrabalhador.GrauInstr);
    if (tipo = 2) then
    begin
      Gerador.wCampo(tcStr, '', 'indPriEmpr  ', 0, 0, 0, eSSimNaoToStr(pTrabalhador.IndPriEmpr));
    end;
    Gerador.wCampo(tcStr, '', 'nmSoc', 0, 0, 0, pTrabalhador.nmSoc);
    if (GroupName = 'trabalhador') then
      GerarNascimento(pTrabalhador.Nascimento);
    GerarDocumentos(pTrabalhador.Documentos);
    GerarEndereco(pTrabalhador.Endereco);
    GerarTrabEstrangeiro(pTrabalhador.TrabEstrangeiro);
    GerarInfoDeficiencia(pTrabalhador.InfoDeficiencia, tipo);
    GerarDependente(pTrabalhador.Dependente);
    if (tipo = 1) or (tipo = 2) then
      GerarAposentadoria(pTrabalhador.Aposentadoria);
    GerarContatoTrabalhador(pTrabalhador.Contato);
  Gerador.wGrupo('/'+GroupName);
end;

procedure TeSocialEvento.GerarTrabEstrangeiro(
  pTrabEstrangeiro: TTrabEstrangeiro);
begin
  if pTrabEstrangeiro.DtChegada > 0 then
  begin
    Gerador.wGrupo('trabEstrangeiro');
      Gerador.wCampo(tcDat, '', 'dtChegada  ', 0, 0, 0, pTrabEstrangeiro.DtChegada);
      Gerador.wCampo(tcStr, '', 'classTrabEstrang  ', 0, 0, 0, eSClassTrabEstrangToStr(pTrabEstrangeiro.classTrabEstrang));
      Gerador.wCampo(tcStr, '', 'casadoBr  ', 0, 0, 0, pTrabEstrangeiro.CasadoBr);
      Gerador.wCampo(tcStr, '', 'filhosBr  ', 0, 0, 0, pTrabEstrangeiro.FilhosBr);
    Gerador.wGrupo('/trabEstrangeiro');
  end;
end;

procedure TeSocialEvento.GerarTrabTemporario(pTrabTemporario: TTrabTemporario);
begin
  if pTrabTemporario.hipLeg > 0 then
  begin
    Gerador.wGrupo('trabTemporario');
      Gerador.wCampo(tcInt, '', 'hipLeg', 0, 0, 0, pTrabTemporario.hipLeg);
      Gerador.wCampo(tcStr, '', 'justContr', 0, 0, 0, pTrabTemporario.justContr);
      Gerador.wCampo(tcInt, '', 'tpInclContr', 0, 0, 0, eSTpInclContrToStr(pTrabTemporario.tpinclContr));
      GerarIdeTomadorServ(pTrabTemporario.IdeTomadorServ);
      GerarIdeTrabSubstituido(pTrabTemporario.IdeTrabSubstituido);
    Gerador.wGrupo('/trabTemporario');
  end;
end;

procedure TeSocialEvento.GerarVinculo(pVinculo: TVinculo; pTipo: integer);
begin
  Gerador.wGrupo('vinculo');
    if (pTipo <> 3) then
      Gerador.wCampo(tcStr, '', 'matricula  ', 0, 0, 0, pVinculo.Matricula);
    Gerador.wCampo(tcStr, '', 'tpRegTrab  ', 0, 0, 0, eSTpRegTrabToStr(pVinculo.TpRegTrab));
    Gerador.wCampo(tcStr, '', 'tpRegPrev  ', 0, 0, 0, eSTpRegPrevToStr(pVinculo.TpRegPrev));

    if (pTipo <> 3) then
    begin
      if (((pVinculo.nrRecInfPrelim <> '') and (pVinculo.nrRecInfPrelim <> null)) and (pTipo = 2)) then
        Gerador.wCampo(tcStr, '', 'nrRecInfPrelim  ', 0, 0, 0, pVinculo.nrRecInfPrelim);
  
  
      GerarInfoRegimeTrab(pVinculo.InfoRegimeTrab);
      GerarInfoContrato(pVinculo.InfoContrato);
      GerarSucessaoVinc(pVinculo.SucessaoVinc);
      if (pTipo = 1) or (pTipo = 2) then
      begin
        GerarAfastamento(pVinculo.Afastamento);
        GerarDesligamento(pVinculo.Desligamento);
      end;
  
      //if (pTipo = 2) then
      //begin
        //GerarInfoASO(pVinculo.InfoASO);
      //end;
    end;
  Gerador.wGrupo('/vinculo');
end;

procedure TeSocialEvento.GerarAfastamento(pAfastamento: TAfastamento);
begin
  if pAfastamento.DtIniAfast > 0 then
  begin
    Gerador.wGrupo('afastamento');
      Gerador.wCampo(tcDat, '', 'dtIniAfast', 0, 0, 0, pAfastamento.DtIniAfast);
      Gerador.wCampo(tcStr, '', 'codMotAfast', 0,0,0, pAfastamento.codMotAfast);
    Gerador.wGrupo('/afastamento');
  end;
end;

procedure TeSocialEvento.GerarAliqGilRat(pAliqRat: TAliqGilRat;
  const GroupName: string);
begin
  Gerador.wGrupo(GroupName);
    Gerador.wCampo(tcStr, '', 'aliqRat', 0, 0, 0, pAliqRat.AliqRat);

    if (pAliqRat.Fap > 0) then
      Gerador.wCampo(tcDe4, '', 'fap', 0, 0, 0, pAliqRat.Fap);

    if (pAliqRat.AliqRatAjust > 0)  then
      Gerador.wCampo(tcDe4, '', 'aliqRatAjust', 0, 0, 0, pAliqRat.AliqRatAjust);

    if pAliqRat.procAdmJudRatInst() then
      GerarProcessoAdmJudRat(pAliqRat.ProcAdmJudRat);
    if pAliqRat.procAdmJudFapInst() then
      GerarProcessoAdmJudFap(pAliqRat.ProcAdmJudFap);
  Gerador.wGrupo('/'+GroupName);
end;

procedure TeSocialEvento.GerarAlvaraJudicial(pAlvaraJudicial: TAlvaraJudicial);
begin
  if pAlvaraJudicial.NrProcJud <> EmptyStr then
  begin
    Gerador.wGrupo('alvaraJudicial');
      Gerador.wCampo(tcStr, '', 'nrProcJud', 0, 0, 0, pAlvaraJudicial.NrProcJud);
    Gerador.wGrupo('/alvaraJudicial');
  end;
end;

procedure TeSocialEvento.GerarAposentadoria(pAposentadoria: TAposentadoria);
begin
  if pAposentadoria.TrabAposent = tpSim then
  begin
    Gerador.wGrupo('aposentadoria');
      Gerador.wCampo(tcStr, '', 'trabAposent', 0, 0, 0, eSSimNaoToStr(pAposentadoria.TrabAposent));
    Gerador.wGrupo('/aposentadoria');
  end;
end;

procedure TeSocialEvento.GerarIdeEmpregador(pEmp: TIdeEmpregador; pNumTags: integer);
begin
  Gerador.wGrupo('ideEmpregador');
    Gerador.wCampo(tcStr, '', 'tpInsc', 0, 0, 1, eSTpInscricaoToStr(pEmp.TpInsc));
    if pEmp.OrgaoPublico then
      Gerador.wCampo(tcStr, '', 'nrInsc', 0, 0, 1, pEmp.NrInsc)
    else
      Gerador.wCampo(tcStr, '', 'nrInsc', 0, 0, 1, Copy(pEmp.NrInsc, 1, 8));
  Gerador.wGrupo('/ideEmpregador');
end;

procedure TeSocialEvento.GerarIdeEvento(pEvt: TIdeEvento; const GeraGrupo: boolean);
begin
  if GeraGrupo then
    Gerador.wGrupo('ideEvento');

  Gerador.wCampo(tcStr, '', 'tpAmb', 0, 0, 1, eStpAmbToStr(pEvt.TpAmb));
  Gerador.wCampo(tcStr, '', 'procEmi', 0, 0, 1, eSProcEmiToStr(pEvt.ProcEmi));
  Gerador.wCampo(tcStr, '', 'verProc', 0, 0, 1, pEvt.VerProc);

  if GeraGrupo then
  	Gerador.wGrupo('/ideEvento');
end;

procedure TeSocialEvento.GerarIdeEvento2(pEvt: TIdeEvento2; const GeraGrupo: boolean = True; GeraRetif: Boolean = True);
begin
  if GeraGrupo then
    Gerador.wGrupo('ideEvento');
    if (GeraRetif) then
      Gerador.wCampo(tcStr, '', 'indRetif', 0, 0, 0, eSIndRetificacaoToStr(pEvt.indRetif));

      if (eSIndRetificacaoToStr(pEvt.indRetif) = '2') then
        Gerador.wCampo(tcStr, '', 'nrRecibo', 0, 0, 0, pEvt.nrRecibo);
{      else
        Gerador.wCampo(tcStr, '', 'nrRecibo', 0, 0, 0, '0');
}
  if GeraGrupo then
    GerarIdeEvento(pEvt, False);

  if GeraGrupo then
    Gerador.wGrupo('/ideEvento');
end;

procedure TeSocialEvento.GerarIdeEvento3(pEvt: TIdeEvento3; IndRetif: Boolean=True);
begin
  Gerador.wGrupo('ideEvento');
    if (indRetif) then
      GerarIdeEvento2(pEvt, false, indRetif);
    Gerador.wCampo(tcStr, '', 'indApuracao', 0, 0, 0, eSIndApuracaoToStr(pEvt.IndApuracao));
    Gerador.wCampo(tcStr, '', 'perApur', 0, 0, 0, pEvt.perApur);
    GerarIdeEvento(pEvt, false);
  Gerador.wGrupo('/ideEvento');
end;

procedure TeSocialEvento.GerarIdeEvento4(pEvt: TIdeEvento4);
begin
  Gerador.wGrupo('ideEvento');
    Gerador.wCampo(tcStr, '', 'indApuracao', 0, 0, 0, eSIndApuracaoToStr(pEvt.IndApuracao));
    Gerador.wCampo(tcStr, '', 'perApur', 0, 0, 0, pEvt.perApur);
    Gerador.wCampo(tcStr, '', 'tpAmb', 0, 0, 1, eStpAmbToStr(pEvt.TpAmb));
    Gerador.wCampo(tcStr, '', 'procEmi', 0, 0, 1, eSProcEmiToStr(pEvt.ProcEmi));
    Gerador.wCampo(tcStr, '', 'verProc', 0, 0, 1, pEvt.VerProc);
  Gerador.wGrupo('/ideEvento');
end;

procedure TeSocialEvento.GerarIdePais(pIdePais: TIdePais);
begin
  Gerador.wGrupo('idePais');
    Gerador.wCampo(tcStr, '', 'codPais  ', 0, 0, 0, pIdePais.codPais);
    Gerador.wCampo(tcStr, '', 'indNIF  ', 0, 0, 0, eSIndNIFToStr(pIdePais.indNIF));
    Gerador.wCampo(tcStr, '', 'nifBenef  ', 0, 0, 0, pIdePais.nifBenef);
  Gerador.wGrupo('/idePais');
end;

procedure TeSocialEvento.GerarIdePeriodo(pIdePeriodo: TidePeriodo;
  const GroupName: string);
begin
  Gerador.wGrupo(GroupName);
    Gerador.wCampo(tcStr, '', 'iniValid', 0, 0, 1, pIdePeriodo.IniValid);
    Gerador.wCampo(tcStr, '', 'fimValid', 0, 0, 0, pIdePeriodo.FimValid);
  Gerador.wGrupo('/'+GroupName);
end;

procedure TeSocialEvento.GerarIdeEstabVinc(pIdeEstabVinc: TIdeEstabVinc);
begin
  if (pIdeEstabVinc.NrInsc <> '') then
  begin
    Gerador.wGrupo('ideEstabVinc');
      Gerador.wCampo(tcStr, '', 'tpInsc', 0, 0, 0, pIdeEstabVinc.TpInsc);
      Gerador.wCampo(tcStr, '', 'nrInsc', 0, 0, 0, pIdeEstabVinc.NrInsc);
    Gerador.wGrupo('/ideEstabVinc');
  end;
end;

procedure TeSocialEvento.GerarIdeTomadorServ(pIdeTomadorServ: TIdeTomadorServ);
begin
  Gerador.wGrupo('ideTomadorServ');
    Gerador.wCampo(tcStr, '', 'tpInsc', 0, 0, 0, pIdeTomadorServ.TpInsc);
    Gerador.wCampo(tcStr, '', 'nrInsc', 0, 0, 0, pIdeTomadorServ.NrInsc);
    GerarIdeEstabVinc(pIdeTomadorServ.ideEstabVinc);
  Gerador.wGrupo('/ideTomadorServ');
end;

procedure TeSocialEvento.GerarIdeTrabSubstituido(
  pIdeTrabSubstituido: TIdeTrabSubstituidoCollection);
Var
  I: Integer;
begin
  for I := 0 to pIdeTrabSubstituido.Count - 1 do
  begin
    Gerador.wGrupo('ideTrabSubstituido');
      Gerador.wCampo(tcStr, '', 'cpfTrabSubst  ', 0, 0, 0, pIdeTrabSubstituido.Items[i].CpfTrabSubst);
    Gerador.wGrupo('/ideTrabSubstituido');
  end;
end;

procedure TeSocialEvento.GerarIdeVinculo(pIdeVinculo: TIdeVinculo);
begin
  Gerador.wGrupo('ideVinculo');
    Gerador.wCampo(tcStr, '', 'cpfTrab  ', 0, 0, 0, pIdeVinculo.cpfTrab);
    Gerador.wCampo(tcStr, '', 'nisTrab  ', 0, 0, 0, pIdeVinculo.nisTrab);
    Gerador.wCampo(tcStr, '', 'matricula  ', 0, 0, 0, pIdeVinculo.matricula);
  Gerador.wGrupo('/ideVinculo');
end;

procedure TeSocialEvento.gerarIdVersao(pIdEsocial: TESocial);
begin
  Gerador.wCampo(tcStr, '', 'id', 0, 0, 0, pIdEsocial.id);
//  Gerador.wCampo(tcStr, '', 'versao', 0, 0, 0, pIdEsocial.versao);
end;

procedure TeSocialEvento.GerarInfoAgNocivo(pInfoAgNocivo: TInfoAgNocivo);
begin
  Gerador.wGrupo('infoAgNocivo');
    Gerador.wCampo(tcStr, '', 'grauExp ', 0, 0, 0, eSGrauExpToStr(pInfoAgNocivo.grauExp));
  Gerador.wGrupo('/infoAgNocivo');
end;

procedure TeSocialEvento.GerarInfoASO(pInfoASO: TInfoASO);
begin
  Gerador.wGrupo('infoASO');
    Gerador.wCampo(tcDat, '', 'dtAsoAdm', 0, 0, 0, pInfoASO.DtAso);
    Gerador.wCampo(tcStr, '', 'nrCRM', 0, 0, 0, pInfoASO.NrCRM);
    Gerador.wCampo(tcStr, '', 'ufCRM', 0, 0, 0, eSufToStr(pInfoASO.UfCRM));
  Gerador.wGrupo('/infoASO');
end;

procedure TeSocialEvento.GerarInfoAtivDesemp(pInfoAtivDesemp: TInfoAtivDesemp);
begin
  Gerador.wGrupo('infoAtivDesemp');
    GerarDescAtividade(pInfoAtivDesemp.DescAtividade);
  Gerador.wGrupo('/infoAtivDesemp');
end;

procedure TeSocialEvento.GerarInfoAprend(pAprend: TAprend);
begin
  if (pAprend.NrInsc <> '') then
  begin
    Gerador.wGrupo('aprend');
      Gerador.wCampo(tcStr, '', 'tpInsc', 0, 0, 0, pAprend.TpInsc);
      Gerador.wCampo(tcStr, '', 'nrInsc', 0, 0, 0, pAprend.NrInsc);
    Gerador.wGrupo('/aprend');
  end;
end;

procedure TeSocialEvento.GerarInfoCeletista(pInfoCeletista: TInfoCeletista);
begin
  if NaoEstaVazio(pInfoCeletista.cnpjSindCategProf) then
  begin
    Gerador.wGrupo('infoCeletista');
  
      Gerador.wCampo(tcDat, '', 'dtAdm ', 0, 0, 0, pInfoCeletista.DtAdm);
      Gerador.wCampo(tcStr, '', 'tpAdmissao ', 0, 0, 0, eSTpAdmissaoToStr(pInfoCeletista.TpAdmissao));
      Gerador.wCampo(tcStr, '', 'indAdmissao ', 0, 0, 0, eSTpIndAdmissaoToStr(pInfoCeletista.IndAdmissao));
  
      Gerador.wCampo(tcStr, '', 'tpRegJor ', 0, 0, 0, eSTpRegJorToStr(pInfoCeletista.TpRegJor));
      Gerador.wCampo(tcStr, '', 'natAtividade ', 0, 0, 0, eSNatAtividadeToStr(pInfoCeletista.NatAtividade));
      if ((pInfoCeletista.dtBase >= 1) and (pInfoCeletista.dtBase <= 12)) then
        Gerador.wCampo(tcStr, '', 'dtBase  ', 0, 0, 0, pInfoCeletista.dtBase);
      Gerador.wCampo(tcStr, '', 'cnpjSindCategProf  ', 0, 0, 0, pInfoCeletista.cnpjSindCategProf);
  
      GerarFGTS(pInfoCeletista.FGTS);
      GerarTrabTemporario(pInfoCeletista.TrabTemporario);
      GerarInfoAprend(pInfoCeletista.aprend);
    Gerador.wGrupo('/infoCeletista');
  end;    
end;

procedure TeSocialEvento.GerarInfoDeficiencia(
  pInfoDeficiencia: TInfoDeficiencia; pTipo: Integer = 0);
begin
  if pInfoDeficiencia.DefFisica = tpSim then
  begin
    Gerador.wGrupo('infoDeficiencia');
      Gerador.wCampo(tcStr, '', 'defFisica ', 0, 0, 0, eSSimNaoToStr(pInfoDeficiencia.DefFisica));
      Gerador.wCampo(tcStr, '', 'defVisual ', 0, 0, 0, eSSimNaoToStr(pInfoDeficiencia.DefVisual));
      Gerador.wCampo(tcStr, '', 'defAuditiva ', 0, 0, 0, eSSimNaoToStr(pInfoDeficiencia.DefAuditiva));
      Gerador.wCampo(tcStr, '', 'defMental ', 0, 0, 0, eSSimNaoToStr(pInfoDeficiencia.DefMental));
      Gerador.wCampo(tcStr, '', 'defIntelectual ', 0, 0, 0, eSSimNaoToStr(pInfoDeficiencia.DefIntelectual));
      Gerador.wCampo(tcStr, '', 'reabReadap  ', 0, 0, 0, eSSimNaoToStr(pInfoDeficiencia.reabReadap));
      if (pTipo <> 3) then
        Gerador.wCampo(tcStr, '', 'infoCota', 0, 0, 0, eSSimNaoToStr(pInfoDeficiencia.infoCota));

      if ((pInfoDeficiencia.Observacao <> '') or (pInfoDeficiencia.Observacao <> null)) then
        Gerador.wCampo(tcStr, '', 'observacao  ', 0, 0, 0, pInfoDeficiencia.Observacao);
    Gerador.wGrupo('/infoDeficiencia');
  end;
end;

procedure TeSocialEvento.GerarInfoDecJud(pInfoDecJud: TInfoDecJud);
begin
  if (pInfoDecJud.nrProcJud <> '') then
  begin
    Gerador.wGrupo('infoDecJud');
      Gerador.wCampo(tcStr, '', 'nrProcJud ', 0, 0, 0, pInfoDecJud.nrProcJud);
    Gerador.wGrupo('/infoDecJud');
  end;
end;

procedure TeSocialEvento.GerarInfoEstatutario(
  pInfoEstatutario: TInfoEstatutario);
begin
  if pInfoEstatutario.DtNomeacao > 0 then
  begin
    Gerador.wGrupo('infoEstatutario');
      Gerador.wCampo(tcStr, '', 'indProvim ', 0, 0, 0, eSIndProvimToStr(pInfoEstatutario.IndProvim));
      Gerador.wCampo(tcStr, '', 'tpProv ', 0, 0, 0,  eSTpProvToStr(pInfoEstatutario.TpProv));
      Gerador.wCampo(tcDat, '', 'dtNomeacao ', 0, 0, 0, pInfoEstatutario.DtNomeacao);
      Gerador.wCampo(tcDat, '', 'dtPosse ', 0, 0, 0, pInfoEstatutario.DtPosse);
  
      if ((eSIndProvimToStr(pInfoEstatutario.IndProvim) = '1') or
          (eSIndProvimToStr(pInfoEstatutario.IndProvim) = '2') or
          (DateToStr(pInfoEstatutario.DtExercicio) <> '')) then
        Gerador.wCampo(tcDat, '', 'dtExercicio ', 0, 0, 0, pInfoEstatutario.DtExercicio);
      Gerador.wCampo(tcInt, '', 'tpPlanRP', 0, 0, 0,  eSTpPlanRPToStr(pInfoEstatutario.tpPlanRP));
      GerarInfoDecJud(pInfoEstatutario.infoDecJud);
    Gerador.wGrupo('/infoEstatutario');
  end;    
end;

procedure TeSocialEvento.GerarInfoRegimeTrab(pInfoRegimeTrab: TInfoRegimeTrab);
begin
  Gerador.wGrupo('infoRegimeTrab');
    GerarInfoCeletista(pInfoRegimeTrab.InfoCeletista);
    GerarInfoEstatutario(pInfoRegimeTrab.InfoEstatutario);
  Gerador.wGrupo('/infoRegimeTrab');
end;

procedure TeSocialEvento.GerarItensRemun(objItensRemun: TRubricaCollection; const GroupName: string = 'verbasResc');
    var
      iItensRemun: Integer;
begin
  for iItensRemun := 0 to objItensRemun.Count - 1 do
  begin
    Gerador.wGrupo(GroupName);
      Gerador.wCampo(tcStr, '', 'codRubr', 0, 0, 0, objItensRemun.Items[iItensRemun].codRubr);
      if objItensRemun.Items[iItensRemun].ideTabRubr <> EmptyStr then
        Gerador.wCampo(tcStr, '', 'ideTabRubr', 1, 1, 1, objItensRemun.Items[iItensRemun].ideTabRubr);
      Gerador.wCampo(tcDe2, '', 'qtdRubr', 0, 0, 0, objItensRemun.Items[iItensRemun].qtdRubr);
      Gerador.wCampo(tcDe2, '', 'fatorRubr', 0, 0, 0, objItensRemun.Items[iItensRemun].fatorRubr);
      Gerador.wCampo(tcDe2, '', 'vrUnit', 0, 0, 0, objItensRemun.Items[iItensRemun].vrUnit);
      Gerador.wCampo(tcDe2, '', 'vrRubr', 0, 0, 0, objItensRemun.Items[iItensRemun].vrRubr);
    Gerador.wGrupo('/' + GroupName);
  end;
end;

procedure TeSocialEvento.GerarLocalTrabalho(pLocalTrabalho: TLocalTrabalho);
begin
  Gerador.wGrupo('localTrabalho');
    GerarLocalTrabGeral(pLocalTrabalho.LocalTrabGeral);
    GerarLocalTrabDom(pLocalTrabalho.LocalTrabDom);
  Gerador.wGrupo('/localTrabalho');
end;

procedure TeSocialEvento.GerarLocalTrabDom(pLocalTrabDom: TLocalTrabDom);
begin
  if NaoEstaVazio(pLocalTrabDom.TpLograd) then
  begin
    Gerador.wGrupo('localTrabDom');
      Gerador.wCampo(tcStr, '', 'tpLograd ', 0, 0, 0, pLocalTrabDom.TpLograd);
      Gerador.wCampo(tcStr, '', 'dscLograd ', 0, 0, 0,  pLocalTrabDom.DscLograd);
      Gerador.wCampo(tcStr, '', 'nrLograd ', 0, 0, 0, pLocalTrabDom.NrLograd);
  
      if (pLocalTrabDom.Complemento <> '') then
        Gerador.wCampo(tcStr, '', 'complemento ', 0, 0, 0,  pLocalTrabDom.Complemento);
  
      if (pLocalTrabDom.Bairro <> '') then
        Gerador.wCampo(tcStr, '', 'bairro ', 0, 0, 0, pLocalTrabDom.Bairro);
  
      Gerador.wCampo(tcStr, '', 'cep ', 0, 0, 0, pLocalTrabDom.Cep);
      Gerador.wCampo(tcStr, '', 'codMunic ', 0, 0, 0,  pLocalTrabDom.CodMunic);
      Gerador.wCampo(tcStr, '', 'uf ', 0, 0, 0, eSufToStr(pLocalTrabDom.Uf));
    Gerador.wGrupo('/localTrabDom');
  end;    
end;

procedure TeSocialEvento.GerarLocalTrabGeral(pLocalTrabGeral: TLocalTrabGeral);
begin
  if NaoEstaVazio(pLocalTrabGeral.NrInsc) then
  begin
    Gerador.wGrupo('localTrabGeral');
      Gerador.wCampo(tcStr, '', 'tpInsc', 0, 0, 1, eSTpInscricaoToStr(pLocalTrabGeral.TpInsc));
      Gerador.wCampo(tcStr, '', 'nrInsc ', 0, 0, 0,  pLocalTrabGeral.NrInsc);
  
      if pLocalTrabGeral.DescComp <> '' then
        Gerador.wCampo(tcStr, '', 'descComp ', 0, 0, 0, pLocalTrabGeral.DescComp);
    Gerador.wGrupo('/localTrabGeral');
  end;  
end;

procedure TeSocialEvento.GerarModoAbertura(pModo: TModoLancamento);
begin
  case pModo of
    mlAlteracao: Gerador.wGrupo('alteracao');
    mlExclusao: Gerador.wGrupo('exclusao');
    else Gerador.wGrupo('inclusao');
  end;
end;

procedure TeSocialEvento.GerarModoFechamento(pModo: TModoLancamento);
begin
  case pModo of
    mlAlteracao: Gerador.wGrupo('/alteracao');
    mlExclusao: Gerador.wGrupo('/exclusao');
    else Gerador.wGrupo('/inclusao');
  end;
end;

procedure TeSocialEvento.GerarNascimento(pNasc: TNascimento; pGroupName: string = 'nascimento');
begin
  Gerador.wGrupo(pGroupName);
    Gerador.wCampo(tcDat, '', 'dtNascto  ', 0, 0, 0, pNasc.DtNascto);

    if pNasc.CodMunic <> 0 then
      Gerador.wCampo(tcStr, '', 'codMunic  ', 0, 0, 0, pNasc.CodMunic);
    Gerador.wCampo(tcStr, '', 'uf  ', 0, 0, 0, pNasc.UF);
    Gerador.wCampo(tcStr, '', 'paisNascto  ', 0, 0, 0, pNasc.PaisNascto);
    Gerador.wCampo(tcStr, '', 'paisNac  ', 0, 0, 0, pNasc.PaisNac);

    if (pNasc.NmMae <> '') then
      Gerador.wCampo(tcStr, '', 'nmMae  ', 0, 0, 0, pNasc.NmMae);

    if (pNasc.NmPai <> '') then
      Gerador.wCampo(tcStr, '', 'nmPai  ', 0, 0, 0, pNasc.NmPai);
  Gerador.wGrupo('/' + pGroupName);
end;

procedure TeSocialEvento.GerarOC(pOc: TOC);
begin
  if NaoEstaVazio(pOc.NrOc) then
  begin
    Gerador.wGrupo('OC');
      Gerador.wCampo(tcStr, '', 'nrOc  ', 0, 0, 0, pOc.NrOc);
      Gerador.wCampo(tcStr, '', 'orgaoEmissor  ', 0, 0, 0, pOc.OrgaoEmissor);
      Gerador.wCampo(tcDat, '', 'dtExped  ', 0, 0, 0, pOc.DtExped);
  
      if (DateToStr(pOc.DtValid) <> '') then
        Gerador.wCampo(tcDat, '', 'dtValid  ', 0, 0, 0, pOc.DtValid);
    Gerador.wGrupo('/OC');
  end;  
end;

procedure TeSocialEvento.GerarPensaoAlim(objPensaoAlim: TPensaoAlimCollection; const GroupName: String = 'pensaoAlim');
var
  iPensaoAlim: Integer;
begin
  for iPensaoAlim := 0 to objPensaoAlim.Count - 1 do
  begin
    Gerador.wGrupo(GroupName);
      Gerador.wCampo(tcStr, '', 'cpfBenef', 0, 0, 0, objPensaoAlim.Items[iPensaoAlim].cpfBenef);
      Gerador.wCampo(tcDat, '', 'dtNasctoBenef', 0, 0, 0, objPensaoAlim.Items[iPensaoAlim].dtNasctoBenef);
      Gerador.wCampo(tcStr, '', 'nmBenefic', 0, 0, 0, objPensaoAlim.Items[iPensaoAlim].nmBenefic);
      Gerador.wCampo(tcDe2, '', 'vlrPensao', 0, 0, 0, objPensaoAlim.Items[iPensaoAlim].vlrPensao);
    Gerador.wGrupo('/'+GroupName);
  end;
end;

procedure TeSocialEvento.GerarProcessoGenerico(pChave: string; pProcesso: TProcesso);
begin
  Gerador.wGrupo(pChave);
    Gerador.wCampo(tcStr, '', 'nrProc', 0, 0, 1, pProcesso.nrProc);
    if trim(pProcesso.codSusp) <> '' then
      Gerador.wCampo(tcStr, '', 'codSusp', 0, 0, 1, pProcesso.codSusp);
  Gerador.wGrupo('/' + pChave);
end;

procedure TeSocialEvento.GerarProcessoAdmJudFap(pProcAdmJudFap: TProcAdmJudFap);
begin
  Gerador.wGrupo('procAdmJudFap');
    Gerador.wCampo(tcStr, '', 'tpProc', 0, 0, 1, eSTpProcessoToStr(pProcAdmJudFap.tpProc));
    Gerador.wCampo(tcStr, '', 'nrProc', 0, 0, 1, pProcAdmJudFap.nrProc);
    if trim(pProcAdmJudFap.codSusp) <> '' then
      Gerador.wCampo(tcStr, '', 'codSusp', 0, 0, 1, pProcAdmJudFap.codSusp);
  Gerador.wGrupo('/procAdmJudFap');
end;

procedure TeSocialEvento.GerarProcessoAdmJudRat(pProcAdmJudRat: TProcAdmJudRat);
begin
  Gerador.wGrupo('procAdmJudRat');
    Gerador.wCampo(tcStr, '', 'tpProc', 0, 0, 0, eSTpProcessoToStr(pProcAdmJudRat.tpProc));
    Gerador.wCampo(tcStr, '', 'nrProc', 0, 0, 0, pProcAdmJudRat.nrProc);
    if trim(pProcAdmJudRat.codSusp) <> '' then
      Gerador.wCampo(tcStr, '', 'codSusp', 0, 0, 1, pProcAdmJudRat.codSusp);
  Gerador.wGrupo('/procAdmJudRat');
end;

procedure TeSocialEvento.GerarProcJudTrab(objProcJudTrab: TProcJudTrabCollection);
var
  iProcJudTrab: Integer;
begin
  for iProcJudTrab := 0 to objProcJudTrab.Count - 1 do
  begin
    Gerador.wGrupo('procJudTrab');
      Gerador.wCampo(tcStr, '', 'tpTrib', 0, 0, 0, eSTpTributoToStr(objProcJudTrab.Items[iProcJudTrab].tpTrib));
      Gerador.wCampo(tcStr, '', 'nrProcJud', 0, 0, 0, objProcJudTrab.Items[iProcJudTrab].nrProcJud);
      Gerador.wCampo(tcStr, '', 'codSusp', 0, 0, 0, objProcJudTrab.Items[iProcJudTrab].codSusp);
    Gerador.wGrupo('/procJudTrab');
  end;
end;

procedure TeSocialEvento.gerarIdeTrabalhador(
  pideTrabalhador: TideTrabalhador; const GeraGrupo: boolean);
begin
  if GeraGrupo then
    Gerador.wGrupo('ideTrabalhador');

      Gerador.wCampo(tcStr, '', 'cpfTrab  ', 0, 0, 0, pideTrabalhador.CpfTrab);

  if GeraGrupo then
    Gerador.wGrupo('/ideTrabalhador');
end;

procedure TeSocialEvento.gerarIdeTrabalhador2(
  pideTrabalhador: TideTrabalhador2; const GeraGrupo: boolean);
begin
  if GeraGrupo then
    Gerador.wGrupo('ideTrabalhador');

      gerarIdeTrabalhador(pideTrabalhador, False);
      Gerador.wCampo(tcStr, '', 'nisTrab  ', 0, 0, 0, pideTrabalhador.nisTrab);

  if GeraGrupo then
    Gerador.wGrupo('/ideTrabalhador');
end;

procedure TeSocialEvento.gerarIdeFolhaPagto(
  pIdeFolhaPagto: TIdeFolhaPagto);
begin
  if pIdeFolhaPagto.perApur <> EmptyStr then
  begin
    Gerador.wGrupo('ideFolhaPagto');
      Gerador.wCampo(tcStr, '', 'indApuracao', 0, 0, 0, eSIndApuracaoToStr(pIdeFolhaPagto.indApuracao));
      Gerador.wCampo(tcStr, '', 'perApur', 0, 0, 0, pIdeFolhaPagto.perApur);
    Gerador.wGrupo('/ideFolhaPagto');
  end;
end;

procedure TeSocialEvento.gerarEmitente(pEmitente: TEmitente);
begin
  Gerador.wGrupo('emitente');
    Gerador.wCampo(tcStr, '', 'nmEmit', 0, 0, 0, pEmitente.nmEmit);
    Gerador.wCampo(tcStr, '', 'ideOC', 0, 0, 0, eSIdeOCToStr(pEmitente.ideOC));
    Gerador.wCampo(tcStr, '', 'nrOc', 0, 0, 0, pEmitente.nrOc);
    Gerador.wCampo(tcStr, '', 'ufOC', 0, 0, 0, eSufToStr(pEmitente.ufOC));
  Gerador.wGrupo('/emitente');
end;

procedure TeSocialEvento.GerarInfoSaudeColet(objInfoSaudeColet: TInfoSaudeColet);
begin
  Gerador.wGrupo('infoSaudeColet');
    GerarDetOper(objInfoSaudeColet.detOper);
  Gerador.wGrupo('/infoSaudeColet');
end;

procedure TeSocialEvento.GerarDetOper(objDetOper: TDetOperCollection);
var
  iDetOper: Integer;
begin
  for iDetOper := 0 to objDetOper.Count - 1 do
  begin
    Gerador.wGrupo('detOper');
      Gerador.wCampo(tcStr, '', 'cnpjOper', 0, 0, 0, objDetOper.Items[iDetOper].cnpjOper);
      Gerador.wCampo(tcStr, '', 'regANS', 0, 0, 0, objDetOper.Items[iDetOper].regANS);
      Gerador.wCampo(tcDe2, '', 'vrPgTit', 0, 0, 0, objDetOper.Items[iDetOper].vrPgTit);
      GerarDetPlano(objDetOper.Items[iDetOper].detPlano);
    Gerador.wGrupo('/detOper');
  end;
end;

procedure TeSocialEvento.GerarDetPlano(objDetPlanoCollection: TDetPlanoCollection);
var
  iDetPlano: Integer;
begin
  for iDetPlano := 0 to objDetPlanoCollection.Count - 1 do
  begin
    Gerador.wGrupo('detPlano');
      Gerador.wCampo(tcStr, '', 'tpDep', 0, 0, 0, objDetPlanoCollection.Items[iDetPlano].tpDep);
      Gerador.wCampo(tcStr, '', 'cpfDep', 0, 0, 0, objDetPlanoCollection.Items[iDetPlano].cpfDep);
      Gerador.wCampo(tcStr, '', 'nmDep', 0, 0, 0, objDetPlanoCollection.Items[iDetPlano].nmDep);
      Gerador.wCampo(tcDat, '', 'dtNascto', 0, 0, 0, objDetPlanoCollection.Items[iDetPlano].dtNascto);
      Gerador.wCampo(tcDe2, '', 'vlrPgDep', 0, 0, 0, objDetPlanoCollection.Items[iDetPlano].vlrPgDep);
    Gerador.wGrupo('/detPlano');
  end;
end;

procedure TeSocialEvento.GerarNfs(pNfs: TNfsColecao);
var
  i: integer;
begin
  for i := 0 to pNfs.Count - 1 do
  begin
    Gerador.wGrupo('nfs');
      Gerador.wCampo(tcStr, '', 'serie',       0, 0, 0, pNfs.Items[i].serie);
      Gerador.wCampo(tcStr, '', 'nrDocto',     0, 0, 0, pNfs.Items[i].nrDocto);
      Gerador.wCampo(tcDat, '', 'dtEmisNF',    0, 0, 0, pNfs.Items[i].dtEmisNF);
      Gerador.wCampo(tcDe2, '', 'vlrBruto',    0, 0, 0, pNfs.Items[i].vlrBruto);
      Gerador.wCampo(tcDe2, '', 'vrCPDescPR',  0, 0, 0, pNfs.Items[i].vrCPDescPR);
      Gerador.wCampo(tcDe2, '', 'vrRatDescPR', 0, 0, 0, pNfs.Items[i].vrRatDescPR);
      Gerador.wCampo(tcDe2, '', 'vrSenarDesc', 0, 0, 0, pNfs.Items[i].vrSenarDesc);
    Gerador.wGrupo('/nfs');
  end;
end;

procedure TeSocialEvento.GerarRemunOutrEmpr(objRemunOutrEmpr: TRemunOutrEmprCollection);
var
  iRemunOutrEmpr: integer;
begin
  for iRemunOutrEmpr := 0 to objRemunOutrEmpr.Count - 1 do
  begin
    if objRemunOutrEmpr.Items[iRemunOutrEmpr].vlrRemunOE > 0 then
    begin
      Gerador.wGrupo('remunOutrEmpr');
      Gerador.wCampo(tcStr, '', 'tpInsc', 0, 0, 0, eSTpInscricaoToStr(objRemunOutrEmpr.Items[iRemunOutrEmpr].tpInsc));
      Gerador.wCampo(tcStr, '', 'nrInsc', 0, 0, 0, objRemunOutrEmpr.Items[iRemunOutrEmpr].nrInsc);
      Gerador.wCampo(tcStr, '', 'codCateg', 0, 0, 0, objRemunOutrEmpr.Items[iRemunOutrEmpr].codCateg);
      Gerador.wCampo(tcDe2, '', 'vlrRemunOE', 0, 0, 0,objRemunOutrEmpr.Items[iRemunOutrEmpr].vlrRemunOE);
      Gerador.wGrupo('/remunOutrEmpr');
    end;
  end;
end;

procedure TeSocialEvento.GerarInfoMV(pInfoMV: TInfoMV);
begin
  if Ord(pInfoMV.indMV) > 0 then
  begin
    Gerador.wGrupo('infoMV');
      Gerador.wCampo(tcStr, '', 'indMV', 0, 0, 0, eSIndMVToStr(pInfoMV.indMV));
      GerarRemunOutrEmpr(pInfoMV.remunOutrEmpr);
    Gerador.wGrupo('/infoMV');
  end;
end;

procedure TeSocialEvento.GerarInfoSimples(obj: TinfoSimples);
begin
  Gerador.wGrupo('infoSimples');
    Gerador.wCampo(tcStr, '', 'indSimples', 0,0,0, obj.indSimples);
  Gerador.wGrupo('/infoSimples');
end;

procedure TeSocialEvento.GerarIdeEstabLot(pIdeEstabLot: TideEstabLotCollection);
var
  i: integer;
begin
  for i := 0 to pIdeEstabLot.Count - 1 do
  begin
    Gerador.wGrupo('ideEstabLot');
      Gerador.wCampo(tcInt, '', 'tpInsc', 0,0,0, eSTpInscricaoToStr(pIdeEstabLot[i].tpInsc));
      Gerador.wCampo(tcStr, '', 'nrInsc', 0,0,0, pIdeEstabLot[i].nrInsc);
      Gerador.wCampo(tcStr, '', 'codLotacao', 0,0,0, pIdeEstabLot[i].codLotacao);
      GerarItensRemun(pIdeEstabLot[i].detVerbas, 'detVerbas');
      if pIdeEstabLot[i].infoSaudeColetInst then
        GerarInfoSaudeColet(pIdeEstabLot[i].infoSaudeColet);
      if pIdeEstabLot[i].infoAgNocivoInst then
        GerarInfoAgNocivo(pIdeEstabLot[i].infoAgNocivo);
      if pIdeEstabLot[i].infoSimplesInst then
        GerarInfoSimples(pIdeEstabLot[i].infoSimples);
    Gerador.wGrupo('/ideEstabLot');
  end;
end;

procedure TeSocialEvento.GerarQuarentena(obj: TQuarentena);
begin
  if obj.dtFimQuar > 0 then
  begin
    Gerador.wGrupo('quarentena');
      Gerador.wCampo(tcDat, '', 'dtFimQuar', 0,0,0, obj.dtFimQuar);
    Gerador.wGrupo('/quarentena');
  end;
end;

procedure TeSocialEvento.GerarIdeRespInf(obj: TIdeRespInf);
begin
  Gerador.wGrupo('ideRespInf');
    Gerador.wCampo(tcStr, '', 'nmResp', 0, 0, 0, obj.nmResp);
    Gerador.wCampo(tcStr, '', 'cpfResp', 0, 0, 0, obj.cpfResp);
    Gerador.wCampo(tcStr, '', 'telefone', 0, 0, 0, obj.telefone);
    Gerador.wCampo(tcStr, '', 'email', 0, 0, 0, obj.email);
  Gerador.wGrupo('/ideRespInf');
end;

end.

