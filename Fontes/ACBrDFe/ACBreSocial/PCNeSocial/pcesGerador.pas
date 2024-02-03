{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Jean Carlo Cantu                                }
{                              Tiago Ravache                                   }
{                              Guilherme Costa                                 }
{                              Leivio Fontenele                                }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}

{$I ACBr.inc}

unit pcesGerador;

interface

uses
  SysUtils, Classes, StrUtils, variants,
  pcnGerador, pcnLeitor, pcnConversao, pcnAuxiliar,
  ACBrDFeConsts,
  pcesCommon, pcesConversaoeSocial;

type
  TeSocialEvento = class(TeSocial)
  private
    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FVersaoDF: TVersaoeSocial;

    FGerador: TGerador;
    FSchema: TeSocialSchema;

//    procedure SetXML(const Value: AnsiString);
    procedure SetXML(const Value: String);
  protected
    {Geradores de Uso Comum}
//    FXML: AnsiString;
    FXML: String;

    procedure GerarCabecalho(const Namespace: String);
    procedure GerarRodape;
    procedure GerarAliqGilRat(pEmp: TIdeEmpregador; pTpInscEstab: tpTpInsc; pAliqRat: TAliqGilRat; const GroupName: string = 'aliqGilRat');
    procedure GerarAlvaraJudicial(pAlvaraJudicial: TAlvaraJudicial);
    procedure GerarAposentadoria(pAposentadoria: TAposentadoria);
    procedure GerarCNH(pCnh: TCNH);
    procedure GerarContatoTrabalhador(pContato: TContatoTrabalhador);
    procedure GerarInfoContrato(pInfoContrato: TInfoContrato; pTipo: Integer; pInfoRegimeTrab: TInfoRegimeTrab);
    procedure GerarObservacoes(pObservacoes: TObservacoesCollection);
    procedure GerarTransfDom(pTransfDom: TTransfDom);
    procedure GerarCTPS(pCTPS: TCTPS);
    procedure GerarDependente(pDependente: TDependenteCollection; pBeneficiario: boolean = false);
    procedure GerarDescAtividade(pDescAtividade: TDescAtividadeCollection);
    procedure GerarDocumentos(pDocumentos: TDocumentos);
    procedure GerarDuracao(pDuracao: TDuracao; pTipo: Integer);
    procedure GerarEndereco(pEndereco: TEndereco; pExterior: boolean = false);
    procedure GerarEnderecoBrasil(pEndereco: TBrasil; const GroupName: string = 'brasil');
    procedure GerarEnderecoExterior(pEndereco: TExterior);
    procedure GerarEnderecoLocalTrabInterm(pEndereco: TBrasil);
    procedure GerarFGTS(pFgts: TFGTS);
    procedure GerarFiliacaoSindical(pFiliacaoSindical: TFiliacaoSindical);
    procedure GerarHorario(pHorario: THorarioCollection);
    procedure GerarHorContratual(pHorContratual: THorContratual);
    procedure GerarIdeEvento(pEvt: TIdeEvento; const GeraGrupo: boolean = True);
    procedure GerarIdeEvento2(pEvt: TIdeEvento2; const GeraGrupo: Boolean=True; GeraIndRetif: Boolean=True; GeraIndGuia: Boolean=False);
    procedure GerarIdeEvento3(pEvt: TIdeEvento3; GeraIndRetif: Boolean=True; GeraIndApuracao: Boolean=True; GeraIndGuia: Boolean=True);
    procedure GerarIdeEvento4(pEvt: TIdeEvento4; GeraIndApuracao: Boolean=True; GeraIndGuia: Boolean=True);
    procedure GerarIdeEvento5(pEvt: TIdeEvento5; nrRecArqBase: Boolean = True; IndApuracao: Boolean = True);
    procedure GerarIdePeriodo(pIdePeriodo: TidePeriodo; const GroupName: string = 'idePeriodo');
    procedure GerarIdeEmpregador(pEmp: TIdeEmpregador);
    procedure GerarIdeTomadorServ(pIdeTomadorServ: TIdeTomadorServ);
    procedure GerarIdeEstabVinc(pIdeEstabVinc: TIdeEstabVinc);
    procedure GerarIdeTrabSubstituido(pIdeTrabSubstituido: TIdeTrabSubstituidoCollection);
    procedure GerarIdVersao(pIdEsocial: TeSocial);
    procedure GerarIdeVinculo2206(pIdeVinculo: TIdeVinculo; pcodCateg: Boolean = True; pCessao: Boolean = False);
    procedure GerarIdeVinculo(pIdeVinculo: TIdeVinculo; pcodCateg: Boolean = True; pCessao: Boolean = False);
    procedure GerarInfoAtivDesemp(pInfoAtivDesemp: TInfoAtivDesemp);
    procedure GerarInfoDeficiencia(pInfoDeficiencia: TInfoDeficiencia; pTipo: integer = 0);
    procedure GerarLocalTrabGeral(pLocalTrabGeral: TLocalTrabGeral);
    procedure GerarLocalTrabDom(pLocalTrabDom: TLocalTrabDom);
    procedure GerarLocalTempDom(pLocalTempDom: TLocalTempDom);
    procedure GerarLocalTrabalho(pLocalTrabalho: TLocalTrabalho);
    procedure GerarModoAbertura(pModo: TModoLancamento);
    procedure GerarModoFechamento(pModo: TModoLancamento);
    procedure GerarNascimento(pNasc: TNascimento; const pGroupName: string = 'nascimento');
    procedure GerarProcessoGenerico(const pChave: string; pProcesso: TProcesso);
    procedure GerarProcessoAdmJudFap(pProcAdmJudFap: TProcAdmJudFap);
    procedure GerarProcessoAdmJudRat(pProcAdmJudRat: TProcAdmJudRat);
    procedure GerarRemuneracao(pRemuneracao: TRemuneracao);
    procedure GerarRG(pRg: TRg);
    procedure GerarRNE(pRNE: TRNE);
    procedure GerarRic(pRic: TRic);
    procedure GerarOC(pOc: TOC);
    procedure GerarSucessaoVinc(pSucessaoVinc: TSucessaoVinc);
    procedure GerarTrabalhador(pTrabalhador: TTrabalhador; AcadIni: tpSimNao;
      const GroupName: string = 'trabalhador'; const Tipo: Integer = 1;
      const Categoria: Integer = 0);
    procedure GerarTrabEstrangeiro(pTrabEstrangeiro: TTrabEstrangeiro);
    procedure GerarTrabImig(pTrabImig: TTrabImig);
    procedure GerarTrabTemporario(pTrabTemporario: TTrabTemporario);
    procedure GerarInfoASO(pInfoASO: TInfoASO);
    procedure GerarMudancaCPF(pMudancaCPF: TmudancaCPF);
    procedure GerarMudancaCPF2(pMudancaCPF: TmudancaCPF2);
    procedure GerarMudancaCPF3(pMudancaCPF: TmudancaCPF3);
    procedure GerarAfastamento(pAfastamento: TAfastamento);
    procedure GerarDesligamento(pDesligamento: TDesligamento);
    procedure GerarVinculo(pVinculo: TVinculo; pTipo: integer = 1);
    procedure GerarInfoRegimeTrab(pInfoRegimeTrab: TInfoRegimeTrab);
    procedure GerarInfoAprend(pAprend: TAprend);
    procedure GerarInfoCeletista(pInfoCeletista: TInfoCeletista);
    procedure GerarInfoDecJud(pInfoDecJud: TInfoDecJud);
    procedure GerarInfoEstatutario(pInfoEstatutario: TInfoEstatutario);
    procedure GerarIdeTrabalhador(pideTrabalhador: TideTrabalhador; const GeraGrupo: boolean = True);
    procedure GerarIdeTrabalhador2(pideTrabalhador: TideTrabalhador2; const GeraGrupo: boolean);
    procedure GerarIdeFolhaPagto(pIdeFolhaPagto: TIdeFolhaPagto);
    procedure GerarIdeFolhaPagto2(pIdeFolhaPagto: TIdeFolhaPagto);
    procedure GerarEmitente(pEmitente: TEmitente; ATipoEvento: TTipoEvento);
    procedure GerarEndExt(pEndExt: TEndExt);
    procedure GerarIdePais(pIdePais: TIdePais);
    procedure GerarInfoAgNocivo(pInfoAgNocivo: TInfoAgNocivo);
    procedure GerarItensRemun(objItensRemun: TRubricaCollection; const GroupName: string = 'verbasResc');
    procedure GerarProcJudTrab(objProcJudTrab: TProcJudTrabCollection; tpTrib: Boolean = True);
    procedure GerarPensaoAlim(objPensaoAlim: TPensaoAlimCollection; const GroupName: String = 'pensaoAlim');
    procedure GerarInfoSaudeColet(objInfoSaudeColet: TInfoSaudeColet);
    procedure GerarDetPlano(objDetPlanoCollection: TDetPlanoCollection);
    procedure GerarDetOper(objDetOper: TDetOperCollection);
    procedure GerarNfs(pNfs: TNfsColecao);
    procedure GerarRemunOutrEmpr(objRemunOutrEmpr: TRemunOutrEmprCollection);
    procedure GerarInfoMV(pInfoMV: TInfoMV);
    procedure GerarInfoSimples(obj: TinfoSimples);
    procedure GerarIdeEstabLot(pIdeEstabLot: TideEstabLotCollection);
    procedure GerarQuarentena(obj: TQuarentena);
    procedure GerarIdeRespInf(obj: TIdeRespInf);
    procedure GerarTreinamentoCapacitacao(objTreiCap: TtreiCapCollection);
    procedure GerarCessao(Obj: Tcessao);
    procedure GerarInfoRRA(obj: TInfoRRA);
    procedure GerarDespProcJud(obj: TDespProcJud);
    procedure GerarIdeAdv(obj: TIdeAdvCollection);

  public
    FACBreSocial: TObject; //alimenta no create
    constructor Create(AACBreSocial: TObject); reintroduce; virtual; //->recebe a instancia da classe TACBreSocial
    destructor Destroy; override;

    function  GerarXML: boolean; virtual;
    procedure SaveToFile(const CaminhoArquivo: string);
    function  Assinar(const XMLEvento, NomeEvento: String): AnsiString;
    function  GerarChaveEsocial(const emissao: TDateTime;
                                const CNPJF: string;
                                sequencial: Integer): String;
    procedure Validar(Schema: TeSocialSchema);

    property Alertas: String read FAlertas;
    property ErroValidacao: String read FErroValidacao;
    property ErroValidacaoCompleto: String read FErroValidacaoCompleto;
    property VersaoDF: TVersaoeSocial read FVersaoDF write FVersaoDF;
    property Gerador: TGerador  read FGerador write FGerador;
    property schema: TeSocialSchema read Fschema write Fschema;
//    property XML: AnsiString read FXML write SetXML;
    property XML: String read FXML write SetXML;
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
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBreSocial, ACBrDFeSSL, ACBrDFeUtil, ACBrDFeConfiguracoes;

{TeSocialEvento}

function TeSocialEvento.Assinar(const XMLEvento, NomeEvento: String): AnsiString;
var
  XMLAss, ArqXML: string;
 {$IFDEF DEBUG}
 NomeEventoArquivo: string;
 {$ENDIF}
begin
  Result := '';

  ArqXML := XMLEvento;

  // XML já deve estar em UTF8, para poder ser assinado //
  ArqXML := NativeStringToUTF8(ArqXML);
  FXMLOriginal := ArqXML;

  with TACBreSocial(FACBreSocial) do
  begin
    if not (SSL.SSLCryptLib in ([cryOpenSSL, cryWinCrypt])) then
      raise EACBreSocialException.Create(ACBRESOCIAL_CErroCryptLib);

    if not (SSL.SSLXmlSignLib in ([xsXmlSec, xsLibXml2])) then
      raise EACBreSocialException.Create(ACBRESOCIAL_CErroSignLib);

    XMLAss := SSL.Assinar(String(ArqXML), 'eSocial', NomeEvento, '', '', '', 'ID');
    FXMLAssinado := XMLAss;
    FXMLOriginal := XMLAss;

    XMLAss := StringReplace(XMLAss, '<' + ENCODING_UTF8_STD + '>', '', [rfReplaceAll]);
    XMLAss := StringReplace(XMLAss, '<' + XML_V01 + '>', '', [rfReplaceAll]);

//    if Configuracoes.Arquivos.Salvar then
//      Gravar(NomeEvento, XMLAss, Configuracoes.Arquivos.PathSalvar);

    Result := AnsiString(XMLAss);

    {$IFDEF DEBUG}
    NomeEventoArquivo := NomeEvento + '.xml';

    if Configuracoes.Arquivos.Salvar then
    begin
      With TStringList.Create do
      try
        Text := XMLAss;
        SaveToFile(IncludeTrailingPathDelimiter(Configuracoes.Arquivos.PathSalvar) + NomeEventoArquivo);
      finally
        Free;
      end;
    end;
    {$ENDIF}
  end;
end;

constructor TeSocialEvento.Create(AACBreSocial: TObject);
begin
  inherited Create;
  if not(AACBreSocial is TACBreSocial) then
  begin
    raise Exception.Create('Parâmetro AACbreSocial precisa ser do tipo TACBreSocial.');
  end;

  FACBreSocial               := AACBreSocial;
  FGerador                   := TGerador.Create;
  FGerador.ArquivoFormatoXML := '';
end;

destructor TeSocialEvento.Destroy;
begin
  FGerador.Free;
  inherited;
end;

procedure TeSocialEvento.SaveToFile(const CaminhoArquivo: string);
var
  lStr: TStringList;
  lFileName: string;
begin
  lFileName := CaminhoArquivo;
  lStr:= TStringList.Create;
  try
    lStr.Text := string(XML);
    lStr.SaveToFile(ChangeFileExt(lFileName,'.xml'));
  finally
    lStr.Free;
  end;
end;

procedure TeSocialEvento.SetXML(const Value: String);
var
  NomeEvento: String;
  Ok: Boolean;
  Leitor: TLeitor;
  typVersaoeSocial: TVersaoeSocial;
begin
  typVersaoeSocial := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
  FXML             := Value;

  if not XmlEstaAssinado(FXML) then
  begin
    NomeEvento := TipoEventoToStrEvento(StringXMLToTipoEvento(Ok, FXML, typVersaoeSocial), typVersaoeSocial);
    FXML       := Assinar(FXML, NomeEvento);

    Leitor := TLeitor.Create;
    try
      Leitor.Grupo := FXML;
      Self.Id      := Leitor.rAtributo('Id=');
    finally
      Leitor.Free;
    end;

    Validar(TipoEventoToSchemaeSocial(StringXMLToTipoEvento(Ok, FXML, typVersaoeSocial), typVersaoeSocial));
  end;
end;

procedure TeSocialEvento.Validar(Schema: TeSocialSchema);
var
  Erro, AXML: String;
  EhValido: Boolean;
  Evento: string;
begin
  AXML := FXMLAssinado;
  Evento := SchemaeSocialToStr(Schema) + '-' +
          VersaoeSocialToStrSchemas(TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF);


  if EstaVazio(AXML) then
  begin
    Assinar(AXML, Evento);
    AXML := FXMLAssinado;
  end;

  with TACBreSocial(FACBreSocial) do
  begin
    EhValido := SSL.Validar(AXML, Configuracoes.Arquivos.PathSchemas + Evento + '.xsd', Erro);

    if not EhValido then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados do evento: ') +
                        Evento + sLineBreak + FAlertas;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      {$IFDEF DEBUG}
      if Configuracoes.Arquivos.Salvar then
      begin
        with TStringList.Create do
        try
          Add(AXML);
          Add('<!--' + FErroValidacaoCompleto + '-->');
          SaveToFile(Configuracoes.Arquivos.PathSalvar+Evento+'_error' +'.xml');
        finally
          Free;
        end;
      end;
      {$ENDIF}

      raise EACBreSocialException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

procedure TeSocialEvento.GerarCabecalho(const Namespace: String);
begin
  with TACBreSocial(FACBreSocial) do
  begin
    SSL.NameSpaceURI := ACBRESOCIAL_NAMESPACE_URI + Namespace + '/' +
                        VersaoeSocialToStrSchemas(Configuracoes.Geral.VersaoDF);

    Gerador.wGrupo(ENCODING_UTF8, '', False);
    Gerador.wGrupo('eSocial xmlns="' + SSL.NameSpaceURI+'"');
  end;
end;

function TeSocialEvento.GerarChaveEsocial(const emissao: TDateTime;
                                          const CNPJF: string;
                                          sequencial: Integer): String;
var
  nAno, nMes, nDia, nHora, nMin, nSeg, nMSeg: Word;
begin
  // já foi carregado, não é necessário gerar novamente
  if (Length(Self.Id)=36) then
  begin
    Result := Self.Id;
    Exit;
  end;
  // Se o usuario informar 0; o código numerico sera gerado de maneira aleatória //
  if sequencial = 0 then
   sequencial := Random(99999);

  DecodeDate(emissao, nAno, nMes, nDia);
  DecodeTime(emissao, nHora, nMin, nSeg, nMSeg);
  Result := 'ID';

  if (Length(CNPJF) in [8,14]) then
    Result := Result + IntToStr(1)
  else
    Result := Result + IntToStr(2);

  with TACBreSocial(FACBreSocial) do
  begin
    if Configuracoes.Geral.TipoEmpregador in [tePessoaFisica,
               teOrgaoPublicoExecutivoFederal, teOrgaoPublicoLegislativoFederal,
               teOrgaoPublicoJudiciarioFederal, teOrgaoPublicoAutonomoFederal] then
      Result := Result + copy(OnlyNumber(CNPJF) + '00000000000000', 1, 14)
    else
      Result := Result + copy(OnlyNumber(Copy(CNPJF, 1, 8)) + '00000000000000', 1, 14);
  end;

  Result := Result + pcnAuxiliar.IntToStrZero(nAno, 4);
  Result := Result + pcnAuxiliar.IntToStrZero(nMes, 2);
  Result := Result + pcnAuxiliar.IntToStrZero(nDia, 2);
  Result := Result + pcnAuxiliar.IntToStrZero(nHora, 2);
  Result := Result + pcnAuxiliar.IntToStrZero(nMin, 2);
  Result := Result + pcnAuxiliar.IntToStrZero(nSeg, 2);
  Result := Result + pcnAuxiliar.IntToStrZero(sequencial, 5);
end;

procedure TeSocialEvento.GerarCNH(pCnh: TCNH);
begin
  if pCnh.nrRegCnh <> EmptyStr then
  begin
    Gerador.wGrupo('CNH');

    Gerador.wCampo(tcStr, '', 'nrRegCnh',      1, 12, 1, pCnh.nrRegCnh);
    Gerador.wCampo(tcDat, '', 'dtExped',      10, 10, 0, pCnh.DtExped);
    Gerador.wCampo(tcStr, '', 'ufCnh',         2,  2, 1, pCnh.ufCnh);
    Gerador.wCampo(tcDat, '', 'dtValid',      10, 10, 1, pCnh.DtValid);
    Gerador.wCampo(tcDat, '', 'dtPriHab',     10, 10, 0, pCnh.dtPriHab);
    Gerador.wCampo(tcStr, '', 'categoriaCnh',  1,  2, 1, eSCnhToStr(pCnh.categoriaCnh));

    Gerador.wGrupo('/CNH');
  end;
end;

procedure TeSocialEvento.GerarContatoTrabalhador(pContato: TContatoTrabalhador);
begin
  Gerador.wGrupo('contato');

  Gerador.wCampo(tcStr, '', 'fonePrinc',       0, 13, 0, pContato.fonePrinc);

  if VersaoDF <= ve02_05_00 then
    Gerador.wCampo(tcStr, '', 'foneAlternat',  0, 13, 0, pContato.foneAlternat);

  Gerador.wCampo(tcStr, '', 'emailPrinc',      0, 60, 0, pContato.emailPrinc);

  if VersaoDF <= ve02_05_00 then
    Gerador.wCampo(tcStr, '', 'emailAlternat', 0, 60, 0, pContato.emailAlternat);

  Gerador.wGrupo('/contato');
end;

procedure TeSocialEvento.GerarInfoContrato(pInfoContrato: TInfoContrato; pTipo: Integer; pInfoRegimeTrab: TInfoRegimeTrab);
begin
  Gerador.wGrupo('infoContrato');

  if VersaoDF <= ve02_05_00 then
  begin
    Gerador.wCampo(tcStr, '', 'codCargo',    0, 30,  0, pInfoContrato.codCargo);
    Gerador.wCampo(tcStr, '', 'codFuncao',   0, 30,  0, pInfoContrato.codFuncao);
  end
  else
  begin
    Gerador.wCampo(tcStr, '', 'nmCargo',     0, 100, 0, pInfoContrato.nmCargo);
    Gerador.wCampo(tcStr, '', 'CBOCargo',    0,   6, 0, pInfoContrato.CBOCargo);
    Gerador.wCampo(tcDat, '', 'dtIngrCargo', 0,  10, 0, pInfoContrato.dtIngrCargo);
    Gerador.wCampo(tcStr, '', 'nmFuncao',    0, 100, 0, pInfoContrato.nmFuncao);
    Gerador.wCampo(tcStr, '', 'CBOFuncao',   0,   6, 0, pInfoContrato.CBOFuncao);
    Gerador.wCampo(tcStr, '', 'acumCargo',   0,   1, 0, eSSimNaoFacultativoToStr(pInfoContrato.acumCargo));
  end;

  Gerador.wCampo(tcInt, '', 'codCateg',    1,  3, 1, pInfoContrato.CodCateg);

  if VersaoDF <= ve02_05_00 then
  begin
    Gerador.wCampo(tcStr, '', 'codCarreira', 0, 30,  0, pInfoContrato.codCarreira);
    Gerador.wCampo(tcDat, '', 'dtIngrCarr',  0, 10,  0, pInfoContrato.dtIngrCarr);
  end;

  if (pInfoContrato.Remuneracao.vrSalFx > 0) or
     (pInfoContrato.Remuneracao.undSalFixo = sfNaoaplicavel) then
    GerarRemuneracao(pInfoContrato.Remuneracao);
  if pInfoContrato.Duracao.tpContr <> PrazoNaoAplicavel then
    GerarDuracao(pInfoContrato.Duracao, pTipo);
  GerarLocalTrabalho(pInfoContrato.LocalTrabalho);

  //Informações do Horário Contratual do Trabalhador. O preenchimento é obrigatório se {tpRegJor} = [1]
  if (NaoEstaVazio(pInfoRegimeTrab.InfoCeletista.cnpjSindCategProf)) then
    begin
      if (pInfoRegimeTrab.InfoCeletista.TpRegJor = rjSubmetidosHorarioTrabalho) then
        GerarHorContratual(pInfoContrato.HorContratual);
    end;

  if VersaoDF <= ve02_05_00 then
    GerarFiliacaoSindical(pInfoContrato.FiliacaoSindical);

  GerarAlvaraJudicial(pInfoContrato.AlvaraJudicial);
  GerarObservacoes(pInfoContrato.observacoes);

  if VersaoDF > ve02_05_00 then
    if pInfoContrato.treiCapInst() then
      GerarTreinamentoCapacitacao(pInfoContrato.treiCap);

  Gerador.wGrupo('/infoContrato');
end;

procedure TeSocialEvento.GerarCTPS(pCTPS: TCTPS);
begin
  if pCTPS.NrCtps = '' then
    Exit;

  Gerador.wGrupo('CTPS');

  Gerador.wCampo(tcStr, '', 'nrCtps',    1, 11, 1, pCTPS.NrCtps);
  Gerador.wCampo(tcStr, '', 'serieCtps', 1,  5, 1, pCTPS.SerieCtps);
  Gerador.wCampo(tcStr, '', 'ufCtps',    2,  2, 1, pCTPS.UfCtps);

  Gerador.wGrupo('/CTPS');
end;

procedure TeSocialEvento.GerarDependente(pDependente: TDependenteCollection; pBeneficiario: boolean = false);
var
  i: integer;
begin
  for i := 0 to pDependente.Count-1 do
  begin
    Gerador.wGrupo('dependente');

    Gerador.wCampo(tcStr, '', 'tpDep',     1,  2, 1, eStpDepToStr(pDependente.Items[i].TpDep));
    Gerador.wCampo(tcStr, '', 'nmDep',     1, 70, 1, pDependente.Items[i].NmDep);
    Gerador.wCampo(tcDat, '', 'dtNascto', 10, 10, 1, pDependente.Items[i].DtNascto);
    Gerador.wCampo(tcStr, '', 'cpfDep',   11, 11, 0, pDependente.Items[i].CpfDep);

    if (VersaoDF >= ve02_05_00) then
      if (pDependente.Items[i].sexoDep = 'F') or (pDependente.Items[i].sexoDep = 'M') then
        Gerador.wCampo(tcStr, '', 'sexoDep',   1,  1, 0, pDependente.Items[i].sexoDep);

    Gerador.wCampo(tcStr, '', 'depIRRF',   1,  1, 1, eSSimNaoToStr(pDependente.Items[i].depIRRF));

    if not(pBeneficiario) then
      Gerador.wCampo(tcStr, '', 'depSF',     1,  1, 1, eSSimNaoToStr(pDependente.Items[i].depSF));

    if (pBeneficiario ) then
      Gerador.wCampo(tcStr, '', 'incFisMen', 1,  1, 1, eSSimNaoToStr(pDependente.Items[i].incFisMen))
    else
      Gerador.wCampo(tcStr, '', 'incTrab',   1,  1, 1, eSSimNaoToStr(pDependente.Items[i].incTrab));

    if VersaoDF >= veS01_02_00 then
      Gerador.wCampo(tcStr, '', 'descrDep',  0, 100, 0, pDependente.Items[i].descrDep);
    
    Gerador.wGrupo('/dependente');
  end;

  if pDependente.Count > 99 then
    Gerador.wAlerta('', 'dependente', 'Lista de Dependentes', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TeSocialEvento.GerarDescAtividade(pDescAtividade: TDescAtividadeCollection);
var
  i: integer;
begin
  for i := 0 to pDescAtividade.Count-1 do
  begin
    Gerador.wGrupo('descAtividade');
      Gerador.wCampo(tcStr, '', 'descAtivDesemp', 0, 0, 0, pDescAtividade.Items[i].DescAtivDesemp);
    Gerador.wGrupo('/descAtividade');
  end;
end;

procedure TeSocialEvento.GerarDesligamento(pDesligamento: TDesligamento);
begin
  if pDesligamento.DtDeslig > 0 then
  begin
    Gerador.wGrupo('desligamento');

    Gerador.wCampo(tcDat, '', 'dtDeslig', 10, 10, 1, pDesligamento.DtDeslig);

    Gerador.wGrupo('/desligamento');
  end;
end;

procedure TeSocialEvento.GerarDocumentos(pDocumentos: TDocumentos);
begin
  if (pDocumentos.CTPS.NrCtps <> EmptyStr) or
     (pDocumentos.Ric.NrRic <> EmptyStr) or
     (pDocumentos.Rg.NrRg <> EmptyStr) or
     (pDocumentos.RNE.NrRne  <> EmptyStr) or
     (pDocumentos.Oc.NrOc <> EmptyStr) or
     (pDocumentos.Cnh.nrRegCnh <> '') then
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
end;

procedure TeSocialEvento.GerarDuracao(pDuracao: TDuracao; pTipo: Integer);
begin
  Gerador.wGrupo('duracao');

  Gerador.wCampo(tcStr, '', 'tpContr', 1, 1, 1, eSTpContrToStr(pDuracao.TpContr));

  if pDuracao.dtTerm <> 0 then
    Gerador.wCampo(tcDat, '', 'dtTerm',    10, 10, 0, pDuracao.dtTerm);

  if (eSTpContrToStr(pDuracao.TpContr) = '2') or (eSTpContrToStr(pDuracao.TpContr) = '3') then
  begin
    if pTipo in  [1,2] then
      Gerador.wCampo(tcStr, '', 'clauAssec',  1,  1, 0, eSSimNaoToStr(pDuracao.clauAssec));
  end;

  if (eSTpContrToStr(pDuracao.TpContr) = '3') then
    Gerador.wCampo(tcStr, '', 'objDet', 1, 255, 1, pDuracao.objDet);

  Gerador.wGrupo('/duracao');
end;

procedure TeSocialEvento.GerarEndereco(pEndereco: TEndereco;
  pExterior: boolean);
begin
  Gerador.wGrupo('endereco');

  if (not pExterior) or (pEndereco.Exterior.PaisResid = '105') or (pEndereco.Exterior.PaisResid = '') then
  begin // Mora no Brasil
     GerarEnderecoBrasil(pEndereco.Brasil);
  end
  else
  begin
     GerarEnderecoExterior(pEndereco.Exterior);
  end;

  Gerador.wGrupo('/endereco');
end;

procedure TeSocialEvento.GerarEnderecoBrasil(pEndereco: TBrasil; const GroupName: string);
begin
  Gerador.wGrupo(GroupName);


  Gerador.wCampo(tcStr, '', 'tpLograd',    1,  4, 0, pEndereco.TpLograd);
  Gerador.wCampo(tcStr, '', 'dscLograd',   1, 80, 1, pEndereco.DscLograd);
  Gerador.wCampo(tcStr, '', 'nrLograd',    1, 10, 1, pEndereco.NrLograd);
  Gerador.wCampo(tcStr, '', 'complemento', 1, 30, 0, pEndereco.Complemento);
  Gerador.wCampo(tcStr, '', 'bairro',      1, 60, 0, pEndereco.Bairro);
  Gerador.wCampo(tcStr, '', 'cep',         1,  8, 1, pEndereco.Cep);
  Gerador.wCampo(tcInt, '', 'codMunic',    7,  7, 1, pEndereco.CodMunic);
  Gerador.wCampo(tcStr, '', 'uf',          2,  2, 1, pEndereco.UF);

  Gerador.wGrupo('/' + GroupName);
end;

procedure TeSocialEvento.GerarEnderecoExterior(pEndereco: TExterior);
begin
  Gerador.wGrupo('exterior');

  Gerador.wCampo(tcStr, '', 'paisResid',   1,  3, 1, pEndereco.PaisResid);
  Gerador.wCampo(tcStr, '', 'dscLograd',   1, 80, 1, pEndereco.DscLograd);
  Gerador.wCampo(tcStr, '', 'nrLograd',    1, 10, 1, pEndereco.NrLograd);
  Gerador.wCampo(tcStr, '', 'complemento', 1, 30, 0, pEndereco.Complemento);
  Gerador.wCampo(tcStr, '', 'bairro',      1, 60, 0, pEndereco.Bairro);
  Gerador.wCampo(tcStr, '', 'nmCid',       1, 50, 1, pEndereco.NmCid);
  Gerador.wCampo(tcStr, '', 'codPostal',   1, 12, 0, pEndereco.CodPostal);

  Gerador.wGrupo('/exterior');
end;

procedure TeSocialEvento.GerarEndExt(pEndExt: TEndExt);
begin
  Gerador.wGrupo('endExt');

  Gerador.wCampo(tcStr, '', 'dscLograd', 1, 80, 1, pEndExt.dscLograd);
  Gerador.wCampo(tcStr, '', 'nrLograd',  1, 10, 0, pEndExt.nrLograd);
  Gerador.wCampo(tcStr, '', 'complem',   1, 30, 0, pEndExt.complem);
  Gerador.wCampo(tcStr, '', 'bairro',    1, 60, 0, pEndExt.bairro);
  Gerador.wCampo(tcStr, '', 'nmCid',     1, 50, 1, pEndExt.nmCid);
  Gerador.wCampo(tcStr, '', 'codPostal', 1, 12, 0, pEndExt.codPostal);

  Gerador.wGrupo('/endExt');
end;

procedure TeSocialEvento.GerarFGTS(pFgts: TFGTS);
begin
  Gerador.wGrupo('FGTS');

  if VersaoDF <= ve02_05_00 then
    Gerador.wCampo(tcStr, '', 'opcFGTS', 1, 1, 1, eSOpcFGTSToStr(pFGTS.OpcFGTS));

//  if (eSOpcFGTSToStr(pFGTS.OpcFGTS) = '1') then
  Gerador.wCampo(tcDat, '', 'dtOpcFGTS', 0, 10, 0, pFgts.DtOpcFGTS);

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

    Gerador.wCampo(tcStr, '', 'cnpjSindTrab', 1, 14, 1, pFiliacaoSindical[i].CnpjSindTrab);

    Gerador.wGrupo('/filiacaoSindical');
  end;

  if pFiliacaoSindical.Count > 2 then
    Gerador.wAlerta('', 'filiacaoSindical', 'Lista de Filiação Sindical', ERR_MSG_MAIOR_MAXIMO + '2');
end;

procedure TeSocialEvento.GerarHorario(pHorario: THorarioCollection);
var
  i: integer;
begin
  for i := 0 to pHorario.Count-1 do
  begin
    Gerador.wGrupo('horario');

    Gerador.wCampo(tcStr, '', 'dia',           1,  1, 1, eSTpDiaToStr(pHorario.Items[i].Dia));
    Gerador.wCampo(tcStr, '', 'codHorContrat', 1, 30, 1, pHorario.Items[i].CodHorContrat);

    Gerador.wGrupo('/horario');
  end;

  if pHorario.Count > 99 then
    Gerador.wAlerta('', 'horario', 'Lista de Diárias do Horário Contatual', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TeSocialEvento.GerarHorContratual(pHorContratual: THorContratual);
begin
  Gerador.wGrupo('horContratual');

  Gerador.wCampo(tcDe2, '', 'qtdHrsSem', 0, 4, 0, pHorContratual.qtdHrsSem);

  if ((VersaoDF <= ve02_05_00) and
      ((pHorContratual.tpJornada = tjJornadaComHorarioDiarioFixoEFolgaFixaDomingo) or
       (pHorContratual.tpJornada = tjJornadaComHorarioDiarioFixoEFolgaFixaExcetoDomingo) or
       (pHorContratual.tpJornada = tjJornadaComHorarioDiarioFixoEFolhaFixaOutroDiaDaSemana) or
       (pHorContratual.tpJornada = tjTurnoIninterruptoDeRevezamento))) then
    Gerador.wAlerta('', 'tpJornada', 'Atenção! Tipo de Jornada não existe na versão ve02_05_00', '')
  else if (VersaoDF > ve02_05_00) and
          (pHorContratual.tpJornada = tjJornadaComHorarioDiarioFolgaFixos) then
    Gerador.wAlerta('', 'tpJornada', 'Atenção! Tipo de Jornada não existe na versão veS01_00_00', '');

  Gerador.wCampo(tcStr, '', 'tpJornada', 1, 1, 1, eSTpJornadaToStr(pHorContratual.tpJornada));

  if VersaoDF <= ve02_05_00 then
   if (eSTpJornadaToStr(pHorContratual.tpJornada) = '9') then
     Gerador.wCampo(tcStr, '', 'dscTpJorn', 0, 100, 0, pHorContratual.dscTpJorn);

  Gerador.wCampo(tcStr, '', 'tmpParc', 1, 1, 1, tpTmpParcToStr(pHorContratual.tmpParc));

  if VersaoDF <= ve02_05_00 then
    GerarHorario(pHorContratual.horario)
  else
  begin
    Gerador.wCampo(tcStr, '', 'horNoturno', 0, 1,   0, eSSimNaoToStr(pHorContratual.horNoturno));
    Gerador.wCampo(tcStr, '', 'dscJorn',    0, 999, 0, pHorContratual.dscJorn);
  end;

  Gerador.wGrupo('/horContratual');
end;

procedure TeSocialEvento.GerarRemuneracao(pRemuneracao: TRemuneracao);
begin
  Gerador.wGrupo('remuneracao');

  Gerador.wCampo(tcDe2, '', 'vrSalFx',    1, 14, 1, pRemuneracao.VrSalFx);
  Gerador.wCampo(tcStr, '', 'undSalFixo', 1,  1, 1, eSUndSalFixoToStr(pRemuneracao.UndSalFixo));

  if (eSUndSalFixoToStr(pRemuneracao.UndSalFixo) = '6') or
     (eSUndSalFixoToStr(pRemuneracao.UndSalFixo) = '7') then
    Gerador.wCampo(tcStr, '', 'dscSalVar', 0, 255, 0, pRemuneracao.DscSalVar);

  Gerador.wGrupo('/remuneracao');
end;

procedure TeSocialEvento.GerarRG(pRg: TRg);
begin
  if pRg.NrRg <> EmptyStr then
  begin
    Gerador.wGrupo('RG');

    Gerador.wCampo(tcStr, '', 'nrRg',          1, 14, 1, pRg.NrRg );
    Gerador.wCampo(tcStr, '', 'orgaoEmissor',  1, 20, 1, pRg.OrgaoEmissor);
    Gerador.wCampo(tcDat, '', 'dtExped',      10, 10, 0, pRg.DtExped);

    Gerador.wGrupo('/RG');
  end;
end;

procedure TeSocialEvento.GerarRic(pRic: TRic);
begin
  if pRic.NrRic <> EmptyStr then
  begin
    Gerador.wGrupo('RIC');

    Gerador.wCampo(tcStr, '', 'nrRic',         1, 14, 1, pRic.NrRic );
    Gerador.wCampo(tcStr, '', 'orgaoEmissor',  1, 20, 1, pRic.OrgaoEmissor);
    Gerador.wCampo(tcDat, '', 'dtExped',      10, 10, 0, pRic.DtExped);

    Gerador.wGrupo('/RIC');
  end;
end;

procedure TeSocialEvento.GerarRNE(pRNE: TRNE);
begin
  if pRNE.NrRne <> EmptyStr then
  begin
    Gerador.wGrupo('RNE');

    Gerador.wCampo(tcStr, '', 'nrRne',         1, 14, 1, pRNE.NrRne );
    Gerador.wCampo(tcStr, '', 'orgaoEmissor',  1, 20, 1, pRNE.OrgaoEmissor);
    Gerador.wCampo(tcDat, '', 'dtExped',      10, 10, 0, pRNE.DtExped);

    Gerador.wGrupo('/RNE');
  end;
end;

procedure TeSocialEvento.GerarRodape;
begin
  Gerador.wGrupo('/eSocial');
end;

procedure TeSocialEvento.GerarSucessaoVinc(pSucessaoVinc: TSucessaoVinc);
begin
  if pSucessaoVinc.nrInsc <> EmptyStr then
  begin
    Gerador.wGrupo('sucessaoVinc');

    if VersaoDF <= ve02_05_00 then
    begin
      Gerador.wCampo(tcStr, '', 'tpInscAnt',      1, 001, 1, eSTpInscricaoToStr(pSucessaoVinc.tpInscAnt));
      Gerador.wCampo(tcStr, '', 'cnpjEmpregAnt', 14, 014, 1, pSucessaoVinc.cnpjEmpregAnt);
    end
    else
    begin
      Gerador.wCampo(tcStr, '', 'tpInsc',         1, 001, 1, eSTpInscricaoToStr(pSucessaoVinc.tpInsc));
      Gerador.wCampo(tcStr, '', 'nrInsc',        14, 014, 1, pSucessaoVinc.nrInsc);
    end;

    Gerador.wCampo(tcStr, '', 'matricAnt',        0, 030, 0, pSucessaoVinc.matricAnt);
    Gerador.wCampo(tcDat, '', 'dtTransf',        10, 010, 1, pSucessaoVinc.dtTransf);
    Gerador.wCampo(tcStr, '', 'observacao',       0, 255, 0, pSucessaoVinc.observacao);

    Gerador.wGrupo('/sucessaoVinc');
  end;
end;

procedure TeSocialEvento.GerarTrabalhador(pTrabalhador: TTrabalhador; AcadIni: tpSimNao;
  const GroupName: string; const tipo: Integer; const Categoria: Integer);
begin
  Gerador.wGrupo(GroupName);

  if (GroupName = 'trabalhador') then
    Gerador.wCampo(tcStr, '', 'cpfTrab', 11, 11, 1, pTrabalhador.CpfTrab);

  if VersaoDF <= ve02_05_00 then
  begin
    // Preencher com o Número de Identificação Social - NIS, o qual pode ser o PIS,
    // PASEP ou NIT.
    // Validação: O preenchimento é obrigatório, exceto se o código de categoria do
    // trabalhador for igual a [901, 903, 904].
    if ((Categoria = 901) or (Categoria = 903) or (Categoria = 904)) then
      Gerador.wCampo(tcStr, '', 'nisTrab', 1, 11, 0, pTrabalhador.NisTrab)
    else
      Gerador.wCampo(tcStr, '', 'nisTrab', 1, 11, 1, pTrabalhador.NisTrab);
  end;

  Gerador.wCampo(tcStr, '', 'nmTrab',  1, 70, 1, pTrabalhador.NmTrab);
  Gerador.wCampo(tcStr, '', 'sexo',    1,  1, 1, pTrabalhador.Sexo);
  Gerador.wCampo(tcInt, '', 'racaCor', 1,  1, 1, pTrabalhador.RacaCor);

  if ((pTrabalhador.EstCiv >= 1) and (pTrabalhador.EstCiv <= 5)) then
    Gerador.wCampo(tcInt, '', 'estCiv', 1, 1, 0, pTrabalhador.EstCiv);

  Gerador.wCampo(tcStr, '', 'grauInstr', 2, 2, 1, pTrabalhador.GrauInstr);

  if VersaoDF <= ve02_05_00 then
    if (tipo = 2) and (AcadIni = tpNao) then
      Gerador.wCampo(tcStr, '', 'indPriEmpr', 1, 1, 0, eSSimNaoToStr(pTrabalhador.IndPriEmpr));

  Gerador.wCampo(tcStr, '', 'nmSoc', 1, 70, 0, pTrabalhador.nmSoc);

  if (GroupName = 'trabalhador') or
     ((GroupName = 'dadosTrabalhador') and (VersaoDF >= ve02_04_02) and (VersaoDF <= ve02_05_00)) then
    GerarNascimento(pTrabalhador.Nascimento)
  else if (GroupName = 'dadosTrabalhador') and (VersaoDF > ve02_05_00) then
    Gerador.wCampo(tcStr, '', 'paisNac',    1,  3, 1, pTrabalhador.PaisNac);

  if VersaoDF <= ve02_05_00 then
    GerarDocumentos(pTrabalhador.Documentos);

  GerarEndereco(pTrabalhador.Endereco,pTrabalhador.ExtrangeiroSN);

  if VersaoDF <= ve02_05_00 then
    GerarTrabEstrangeiro(pTrabalhador.TrabEstrangeiro)
  else
    GerarTrabImig(pTrabalhador.TrabImig);

  GerarInfoDeficiencia(pTrabalhador.InfoDeficiencia, tipo);
  GerarDependente(pTrabalhador.Dependente);

  if ((tipo = 1) or (tipo = 2)) and (VersaoDF <= ve02_05_00) then
    GerarAposentadoria(pTrabalhador.Aposentadoria);

  GerarContatoTrabalhador(pTrabalhador.Contato);

  Gerador.wGrupo('/'+GroupName);
end;

procedure TeSocialEvento.GerarTrabEstrangeiro(pTrabEstrangeiro: TTrabEstrangeiro);
begin
  if pTrabEstrangeiro.DtChegada > 0 then
  begin
    Gerador.wGrupo('trabEstrangeiro');

    Gerador.wCampo(tcDat, '', 'dtChegada',        10, 10, 1, pTrabEstrangeiro.DtChegada);
    Gerador.wCampo(tcStr, '', 'classTrabEstrang',  1,  2, 1, eSClassTrabEstrangToStr(pTrabEstrangeiro.classTrabEstrang));
    Gerador.wCampo(tcStr, '', 'casadoBr',          1,  1, 1, pTrabEstrangeiro.CasadoBr);
    Gerador.wCampo(tcStr, '', 'filhosBr',          1,  1, 1, pTrabEstrangeiro.FilhosBr);

    Gerador.wGrupo('/trabEstrangeiro');
  end;
end;

procedure TeSocialEvento.GerarTrabImig(pTrabImig: TTrabImig);
begin
  if pTrabImig.condIng <> tciNenhum then
  begin
    Gerador.wGrupo('trabImig');

    Gerador.wCampo(tcStr, '', 'tmpResid',  1,  1, 0, tpTmpResidToStr(pTrabImig.tmpResid));
    Gerador.wCampo(tcStr, '', 'condIng',   1,  1, 1, tpCondIngToStr(pTrabImig.condIng));

    Gerador.wGrupo('/trabImig');
  end;
end;

procedure TeSocialEvento.GerarTrabTemporario(pTrabTemporario: TTrabTemporario);
begin
  if pTrabTemporario.hipLeg > 0 then
  begin
    Gerador.wGrupo('trabTemporario');

    Gerador.wCampo(tcInt, '', 'hipLeg',      1,   1, 1, pTrabTemporario.hipLeg);
    Gerador.wCampo(tcStr, '', 'justContr',   1, 999, 1, pTrabTemporario.justContr);

    if VersaoDF <= ve02_05_00 then
      GerarIdeTomadorServ(pTrabTemporario.ideTomadorServ)
    else
      GerarIdeEstabVinc(pTrabTemporario.ideEstabVinc);

    GerarIdeTrabSubstituido(pTrabTemporario.ideTrabSubstituido);

    Gerador.wGrupo('/trabTemporario');
  end;
end;

procedure TeSocialEvento.GerarVinculo(pVinculo: TVinculo; pTipo: integer);
begin
  Gerador.wGrupo('vinculo');

  if (pTipo <> 3) then
  begin
    Gerador.wCampo(tcStr, '', 'matricula', 1, 30, 1, pVinculo.matricula);
    Gerador.wCampo(tcStr, '', 'tpRegTrab', 1,  1, 1, eSTpRegTrabToStr(pVinculo.tpRegTrab));
  end;

  Gerador.wCampo(tcStr, '', 'tpRegPrev', 1, 1, 1, eSTpRegPrevToStr(pVinculo.tpRegPrev));

  if (pTipo <> 3) then
  begin
    if VersaoDF <= ve02_05_00 then
      if (((pVinculo.nrRecInfPrelim <> EmptyStr) and (pVinculo.nrRecInfPrelim <> null)) and (pTipo = 2)) then
        Gerador.wCampo(tcStr, '', 'nrRecInfPrelim', 0, 40, 0, pVinculo.nrRecInfPrelim);

    Gerador.wCampo(tcStr, '', 'cadIni', 1, 1, 1, eSSimNaoToStr(pVinculo.cadIni));

    GerarInfoRegimeTrab(pVinculo.infoRegimeTrab);

    GerarInfoContrato(pVinculo.infoContrato, pTipo, pVinculo.infoRegimeTrab);
    GerarSucessaoVinc(pVinculo.sucessaoVinc);
    GerarTransfDom(pVinculo.transfDom);

    if (pTipo = 2) then
      GerarMudancaCPF(pVinculo.mudancaCPF);

    if (pTipo = 1) or (pTipo = 2) then
    begin
      GerarAfastamento(pVinculo.afastamento);
      GerarDesligamento(pVinculo.desligamento);

      if VersaoDF >= ve02_05_00 then
        GerarCessao(pVinculo.cessao);
    end;
  end;

  Gerador.wGrupo('/vinculo');
end;

procedure TeSocialEvento.GerarMudancaCPF(pMudancaCPF: TmudancaCPF);
begin
  if VersaoDF >= ve02_05_00 then
  begin
    if pMudancaCPF.cpfAnt <> EmptyStr then
    begin
      Gerador.wGrupo('mudancaCPF');
      Gerador.wCampo(tcStr, '', 'cpfAnt',     11, 11, 1, pMudancaCPF.cpfAnt);
      Gerador.wCampo(tcStr, '', 'matricAnt',   1, 11, 1, pMudancaCPF.matricAnt);
      Gerador.wCampo(tcDat, '', 'dtAltCPF',   10, 10, 1, pMudancaCPF.dtAltCPF);
      Gerador.wCampo(tcStr, '', 'observacao',  1, 11, 0, pMudancaCPF.observacao);
      Gerador.wGrupo('/mudancaCPF');
    end;
  end;
end;

procedure TeSocialEvento.GerarMudancaCPF2(pMudancaCPF: TmudancaCPF2);
begin
  if VersaoDF >= ve02_05_00 then
  begin
    if pMudancaCPF.cpfAnt <> EmptyStr then
    begin
      Gerador.wGrupo('mudancaCPF');

      Gerador.wCampo(tcStr, '', 'cpfAnt',       11, 11, 1, pMudancaCPF.cpfAnt);

      if (VersaoDF > ve02_05_00) and (pMudancaCPF.matricAnt <> EmptyStr) then
        Gerador.wCampo(tcDat, '', 'matricAnt',  30, 30, 0, pMudancaCPF.matricAnt);

      Gerador.wCampo(tcDat, '', 'dtAltCPF',     10, 10, 1, pMudancaCPF.dtAltCPF);
      Gerador.wCampo(tcStr, '', 'observacao',    1, 11, 0, pMudancaCPF.observacao);

      Gerador.wGrupo('/mudancaCPF');
    end;
  end;
end;

procedure TeSocialEvento.GerarMudancaCPF3(pMudancaCPF: TmudancaCPF3);
begin
  if VersaoDF >= ve02_05_00 then
  begin
    if pMudancaCPF.novoCPF <> EmptyStr then
    begin
      Gerador.wGrupo('mudancaCPF');
      Gerador.wCampo(tcStr, '', 'novoCPF', 11, 11, 1, pMudancaCPF.novoCPF);
      Gerador.wGrupo('/mudancaCPF');
    end;
  end;
end;

procedure TeSocialEvento.GerarAfastamento(pAfastamento: TAfastamento);
begin
  if pAfastamento.dtIniAfast > 0 then
  begin
    Gerador.wGrupo('afastamento');

    Gerador.wCampo(tcDat, '', 'dtIniAfast',  10, 10, 1, pAfastamento.dtIniAfast);
    Gerador.wCampo(tcStr, '', 'codMotAfast',  1,  1, 1, eStpMotivosAfastamentoToStr(pAfastamento.codMotAfast));

    Gerador.wGrupo('/afastamento');
  end;
end;

procedure TeSocialEvento.GerarAliqGilRat(pEmp: TIdeEmpregador; pTpInscEstab: tpTpInsc; pAliqRat: TAliqGilRat;
  const GroupName: string);
var
  bProcJudRat: Boolean;
  bProcJudFap: Boolean;
begin
  bProcJudRat := False;
  bProcJudFap := False;

  if pAliqRat.procAdmJudRatInst() then
    if pAliqRat.ProcAdmJudRat.nrProc <> EmptyStr then
      bProcJudRat := True;

  if pAliqRat.procAdmJudFapInst() then
    if pAliqRat.ProcAdmJudFap.nrProc <> EmptyStr then
      bProcJudFap := True;

  if (VersaoDF >= veS01_00_00) and (not bProcJudRat) and (not bProcJudFap) and
     (pTpInscEstab <> tiCNO) and (pAliqRat.Fap <= 0) then
    Exit;

  Gerador.wGrupo(GroupName);

  if (VersaoDF <= ve02_05_00) or bProcJudRat then
    Gerador.wCampo(tcStr, '', 'aliqRat', 1, 1, 1, eSAliqRatToStr(pAliqRat.AliqRat));

  if (pEmp.TpInsc = tiCNPJ) then
  begin
    if (pAliqRat.Fap > 0) or bProcJudFap or (pTpInscEstab = tiCNO) then
      Gerador.wCampo(tcDe4, '', 'fap',          1, 5, 0, pAliqRat.Fap);

    if (VersaoDF <= ve02_05_00) then
       Gerador.wCampo(tcDe4, '', 'aliqRatAjust', 1, 5, 0, pAliqRat.AliqRatAjust);
  end;

  if pAliqRat.procAdmJudRatInst() then
    GerarProcessoAdmJudRat(pAliqRat.ProcAdmJudRat);

  if pAliqRat.procAdmJudFapInst() then
    GerarProcessoAdmJudFap(pAliqRat.ProcAdmJudFap);

  Gerador.wGrupo('/' + GroupName);

end;

procedure TeSocialEvento.GerarAlvaraJudicial(pAlvaraJudicial: TAlvaraJudicial);
begin
  if pAlvaraJudicial.NrProcJud <> EmptyStr then
  begin
    Gerador.wGrupo('alvaraJudicial');

    Gerador.wCampo(tcStr, '', 'nrProcJud', 1, 20, 1, pAlvaraJudicial.NrProcJud);

    Gerador.wGrupo('/alvaraJudicial');
  end;
end;

procedure TeSocialEvento.GerarAposentadoria(pAposentadoria: TAposentadoria);
begin
  if pAposentadoria.TrabAposent = tpSim then
  begin
    Gerador.wGrupo('aposentadoria');

    Gerador.wCampo(tcStr, '', 'trabAposent', 1, 1, 1, eSSimNaoToStr(pAposentadoria.TrabAposent));

    Gerador.wGrupo('/aposentadoria');
  end;
end;

procedure TeSocialEvento.GerarIdeEmpregador(pEmp: TIdeEmpregador);
begin
  Gerador.wGrupo('ideEmpregador');

  Gerador.wCampo(tcStr, '', 'tpInsc', 1, 1, 1, eSTpInscricaoToStr(pEmp.TpInsc));

  if (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador in [tePessoaFisica,
                                                                        teOrgaoPublicoExecutivoFederal, teOrgaoPublicoLegislativoFederal,
                                                                        teOrgaoPublicoJudiciarioFederal, teOrgaoPublicoAutonomoFederal]) then
    Gerador.wCampo(tcStr, '', 'nrInsc', 14, 14, 1, pEmp.NrInsc)
  else
    Gerador.wCampo(tcStr, '', 'nrInsc', 8, 8, 1, Copy(pEmp.NrInsc, 1, 8));

  Gerador.wGrupo('/ideEmpregador');
end;

procedure TeSocialEvento.GerarIdeEvento(pEvt: TIdeEvento; const GeraGrupo: boolean);
begin
  if GeraGrupo then
    Gerador.wGrupo('ideEvento');

  Gerador.wCampo(tcStr, '', 'tpAmb',   1,  1, 1, TpAmbToStr(TACBreSocial(FACBreSocial).Configuracoes.WebServices.Ambiente));
  Gerador.wCampo(tcStr, '', 'procEmi', 1,  1, 1, eSProcEmiToStr(pEvt.ProcEmi));
  Gerador.wCampo(tcStr, '', 'verProc', 1, 20, 1, pEvt.VerProc);

  if GeraGrupo then
  	Gerador.wGrupo('/ideEvento');
end;

procedure TeSocialEvento.GerarIdeEvento2(pEvt: TIdeEvento2; const GeraGrupo: Boolean=True; GeraIndRetif: Boolean=True; GeraIndGuia: Boolean=False);
begin
  if GeraGrupo then
    Gerador.wGrupo('ideEvento');

  if (GeraIndRetif) then
    Gerador.wCampo(tcStr, '', 'indRetif', 1, 1, 1, eSIndRetificacaoToStr(pEvt.indRetif));

  if (eSIndRetificacaoToStr(pEvt.indRetif) = '2') then
    Gerador.wCampo(tcStr, '', 'nrRecibo', 1, 40, 0, pEvt.nrRecibo);

  if (GeraIndGuia) and (VersaoDF >= veS01_00_00) and (pEvt.indGuia <> '') then
    Gerador.wCampo(tcStr, '', 'indGuia', 1, 1, 0, pEvt.indGuia);

  if GeraGrupo then
    GerarIdeEvento(pEvt, False);

  if GeraGrupo then
    Gerador.wGrupo('/ideEvento');
end;

procedure TeSocialEvento.GerarIdeEvento3(pEvt: TIdeEvento3; GeraIndRetif: Boolean=True; GeraIndApuracao: Boolean=True; GeraIndGuia: Boolean=True);
begin
  Gerador.wGrupo('ideEvento');

  GerarIdeEvento2(pEvt, false, GeraIndRetif, false);

  if (GeraIndApuracao) then
    Gerador.wCampo(tcStr, '', 'indApuracao', 1, 1, 1, eSIndApuracaoToStr(pEvt.IndApuracao));

  Gerador.wCampo(tcStr, '', 'perApur',     7, 7, 1, pEvt.perApur);

  if (GeraIndGuia) and (VersaoDF >= veS01_00_00) and (pEvt.indGuia <> '') then
    Gerador.wCampo(tcStr, '', 'indGuia', 1, 1, 0, pEvt.indGuia);

  GerarIdeEvento(pEvt, false);

  Gerador.wGrupo('/ideEvento');
end;

procedure TeSocialEvento.GerarIdeEvento4(pEvt: TIdeEvento4; GeraIndApuracao: Boolean=True; GeraIndGuia: Boolean=True);
begin
  Gerador.wGrupo('ideEvento');

  if (GeraIndApuracao) then
    Gerador.wCampo(tcStr, '', 'indApuracao', 1, 1, 1, eSIndApuracaoToStr(pEvt.IndApuracao));

  Gerador.wCampo(tcStr, '', 'perApur',     7, 7, 1, pEvt.perApur);

  if (GeraIndGuia) and (VersaoDF >= veS01_00_00) and (pEvt.indGuia <> '') then
    Gerador.wCampo(tcStr, '', 'indGuia', 1, 1, 0, pEvt.indGuia);

  GerarIdeEvento(pEvt, false);

  Gerador.wGrupo('/ideEvento');
end;

procedure TeSocialEvento.GerarIdeEvento5(pEvt: TIdeEvento5; nrRecArqBase,
  IndApuracao: Boolean);
begin
  Gerador.wGrupo('ideEvento');

  if nrRecArqBase then
    Gerador.wCampo(tcStr, '', 'nrRecArqBase', 1, 40, 0, pEvt.nrRecArqBase);

  if IndApuracao then
    Gerador.wCampo(tcStr, '', 'indApuracao', 1, 1, 1, eSIndApuracaoToStr(pEvt.IndApuracao));

  Gerador.wCampo(tcStr, '', 'perApur', 7, 7, 1, pEvt.perApur);

  Gerador.wGrupo('/ideEvento');
end;

procedure TeSocialEvento.GerarIdePais(pIdePais: TIdePais);
begin
  Gerador.wGrupo('idePais');

  Gerador.wCampo(tcStr, '', 'codPais',  1,  3, 1, pIdePais.codPais);
  Gerador.wCampo(tcStr, '', 'indNIF',   1,  1, 1, eSIndNIFToStr(pIdePais.indNIF));
  Gerador.wCampo(tcStr, '', 'nifBenef', 1, 20, 0, pIdePais.nifBenef);

  Gerador.wGrupo('/idePais');
end;

procedure TeSocialEvento.GerarIdePeriodo(pIdePeriodo: TidePeriodo;
  const GroupName: string);
begin
  Gerador.wGrupo(GroupName);

  Gerador.wCampo(tcStr, '', 'iniValid', 7, 7, 1, pIdePeriodo.IniValid);
  Gerador.wCampo(tcStr, '', 'fimValid', 7, 7, 0, pIdePeriodo.FimValid);

  Gerador.wGrupo('/' + GroupName);
end;

procedure TeSocialEvento.GerarIdeEstabVinc(pIdeEstabVinc: TIdeEstabVinc);
begin
  if (pIdeEstabVinc.NrInsc <> EmptyStr) then
  begin
    Gerador.wGrupo('ideEstabVinc');

    Gerador.wCampo(tcStr, '', 'tpInsc', 1,  1, 1, eSTpInscricaoToStr(pIdeEstabVinc.TpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc', 1, 15, 1, pIdeEstabVinc.NrInsc);

    Gerador.wGrupo('/ideEstabVinc');
  end;
end;

procedure TeSocialEvento.GerarIdeTomadorServ(pIdeTomadorServ: TIdeTomadorServ);
begin
  if VersaoDF <= ve02_05_00 then
  begin
    Gerador.wGrupo('ideTomadorServ');

    Gerador.wCampo(tcStr, '', 'tpInsc', 1,  1, 1, eSTpInscricaoToStr(pIdeTomadorServ.TpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc', 1, 15, 1, pIdeTomadorServ.NrInsc);
  end;

  GerarIdeEstabVinc(pIdeTomadorServ.ideEstabVinc);

  if VersaoDF <= ve02_05_00 then
    Gerador.wGrupo('/ideTomadorServ');
end;

procedure TeSocialEvento.GerarIdeTrabSubstituido(
  pIdeTrabSubstituido: TIdeTrabSubstituidoCollection);
var
  I: Integer;
begin
  for I := 0 to pIdeTrabSubstituido.Count - 1 do
  begin
    Gerador.wGrupo('ideTrabSubstituido');

    Gerador.wCampo(tcStr, '', 'cpfTrabSubst  ', 11, 11, 1, pIdeTrabSubstituido.Items[i].CpfTrabSubst);

    Gerador.wGrupo('/ideTrabSubstituido');
  end;

  if pIdeTrabSubstituido.Count > 9 then
    Gerador.wAlerta('', 'ideTrabSubstituido', 'Lista de Trabalhadores Substituido', ERR_MSG_MAIOR_MAXIMO + '9');
end;

procedure TeSocialEvento.GerarIdeVinculo2206(pIdeVinculo: TIdeVinculo; pcodCateg: Boolean = True; pCessao: Boolean = False);
begin
  Gerador.wGrupo('ideVinculo');

  Gerador.wCampo(tcStr, '', 'cpfTrab', 11, 11, 1, pIdeVinculo.cpfTrab);

  if not(pCessao) then
    if VersaoDF <= ve02_05_00 then
    begin
      if ((pIdeVinculo.codCateg = 901) or (pIdeVinculo.codCateg = 903) or (pIdeVinculo.codCateg = 904)) then
        Gerador.wCampo(tcStr, '', 'nisTrab', 1, 11, 0, pIdeVinculo.nisTrab)
      else
        Gerador.wCampo(tcStr, '', 'nisTrab', 1, 11, 1, pIdeVinculo.nisTrab);
    end;

  if (IntToTpProf(pIdeVinculo.codCateg) = ttpProfissionalEmpregado) then
   Gerador.wCampo(tcStr, '', 'matricula', 1, 30, 0, pIdeVinculo.matricula);

  if not(pCessao) then
    if (pcodCateg) then
      Gerador.wCampo(tcInt, '', 'codCateg',  3,  3, 0, pIdeVinculo.codCateg);

  Gerador.wGrupo('/ideVinculo');
end;

procedure TeSocialEvento.GerarIdeVinculo(pIdeVinculo: TIdeVinculo; pcodCateg: Boolean = True; pCessao: Boolean = False);
begin
  Gerador.wGrupo('ideVinculo');

  Gerador.wCampo(tcStr, '', 'cpfTrab', 11, 11, 1, pIdeVinculo.cpfTrab);

  if not(pCessao) then
    if VersaoDF <= ve02_05_00 then
    begin
      if ((pIdeVinculo.codCateg = 901) or (pIdeVinculo.codCateg = 903) or (pIdeVinculo.codCateg = 904)) then
        Gerador.wCampo(tcStr, '', 'nisTrab', 1, 11, 0, pIdeVinculo.nisTrab)
      else
        Gerador.wCampo(tcStr, '', 'nisTrab', 1, 11, 1, pIdeVinculo.nisTrab);
    end;

  Gerador.wCampo(tcStr, '', 'matricula', 0, 30, 0, pIdeVinculo.matricula);

  if not(pCessao) then
    if (pcodCateg) then
      Gerador.wCampo(tcInt, '', 'codCateg',  3,  3, 0, pIdeVinculo.codCateg);

  Gerador.wGrupo('/ideVinculo');
end;

procedure TeSocialEvento.GerarIdVersao(pIdEsocial: TESocial);
begin
  Gerador.wCampo(tcStr, '', 'id', 0, 0, 0, pIdEsocial.id);
//  Gerador.wCampo(tcStr, '', 'versao', 0, 0, 0, pIdEsocial.versao);
end;

procedure TeSocialEvento.GerarInfoAgNocivo(pInfoAgNocivo: TInfoAgNocivo);
begin
  Gerador.wGrupo('infoAgNocivo');

  Gerador.wCampo(tcStr, '', 'grauExp', 1, 1, 1, eSGrauExpToStr(pInfoAgNocivo.grauExp));

  Gerador.wGrupo('/infoAgNocivo');
end;

procedure TeSocialEvento.GerarInfoASO(pInfoASO: TInfoASO);
begin
  Gerador.wGrupo('infoASO');

  Gerador.wCampo(tcDat, '', 'dtAsoAdm', 0, 0, 0, pInfoASO.DtAso);
  Gerador.wCampo(tcStr, '', 'nrCRM',    0, 0, 0, pInfoASO.NrCRM);
  Gerador.wCampo(tcStr, '', 'ufCRM',    0, 0, 0, pInfoASO.UfCRM);

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
  if (pAprend.NrInsc <> EmptyStr) or (pAprend.cnpjEntQual <> EmptyStr) then
  begin
    Gerador.wGrupo('aprend');

    if VersaoDF >= veS01_02_00 then
    begin
      Gerador.wCampo(tcStr, '', 'indAprend',   1,  1, 1, eStpIndAprendToStr(pAprend.indAprend));
      Gerador.wCampo(tcStr, '', 'cnpjEntQual', 0, 15, 0, pAprend.cnpjEntQual);
    end;

    if pAprend.NrInsc <> EmptyStr then
    begin
      Gerador.wCampo(tcStr, '', 'tpInsc', 1,  1, 1, eSTpInscricaoToStr(pAprend.TpInsc));
      Gerador.wCampo(tcStr, '', 'nrInsc', 1, 15, 1, pAprend.NrInsc);
    end;

    if VersaoDF >= veS01_02_00 then
      Gerador.wCampo(tcStr, '', 'cnpjPrat', 0, 15, 0, pAprend.cnpjPrat);

    Gerador.wGrupo('/aprend');
  end;
end;

procedure TeSocialEvento.GerarInfoCeletista(pInfoCeletista: TInfoCeletista);
begin
  if NaoEstaVazio(pInfoCeletista.cnpjSindCategProf) then
  begin
    Gerador.wGrupo('infoCeletista');

    Gerador.wCampo(tcDat, '', 'dtAdm',        10, 10, 1, pInfoCeletista.DtAdm);
    Gerador.wCampo(tcStr, '', 'tpAdmissao',    1,  1, 1, eSTpAdmissaoToStr(pInfoCeletista.TpAdmissao));
    Gerador.wCampo(tcStr, '', 'indAdmissao',   1,  1, 1, eSTpIndAdmissaoToStr(pInfoCeletista.IndAdmissao));
    if VersaoDF > ve02_05_00 then
      Gerador.wCampo(tcStr, '', 'nrProcTrab',    1,  20,0, pInfoCeletista.nrProcTrab);
    Gerador.wCampo(tcStr, '', 'tpRegJor',      1,  1, 1, eSTpRegJorToStr(pInfoCeletista.TpRegJor));
    Gerador.wCampo(tcStr, '', 'natAtividade',  1,  1, 1, eSNatAtividadeToStr(pInfoCeletista.NatAtividade));

    if ((pInfoCeletista.dtBase >= 1) and (pInfoCeletista.dtBase <= 12)) then
      Gerador.wCampo(tcInt, '', 'dtBase',      1,  2, 1, pInfoCeletista.dtBase);

    Gerador.wCampo(tcStr, '', 'cnpjSindCategProf', 14, 14, 1, pInfoCeletista.cnpjSindCategProf);

    if (VersaoDF >= veS01_02_00) and (pInfoCeletista.matAnotJud <> '') then
      Gerador.wCampo(tcStr, '', 'matAnotJud',  1, 30, 1, pInfoCeletista.matAnotJud);
    
    if (pInfoCeletista.FGTS.DtOpcFGTS > 0) then
      GerarFGTS(pInfoCeletista.FGTS);

    GerarTrabTemporario(pInfoCeletista.TrabTemporario);
    GerarInfoAprend(pInfoCeletista.aprend);

    Gerador.wGrupo('/infoCeletista');
  end;
end;

procedure TeSocialEvento.GerarInfoDeficiencia(
  pInfoDeficiencia: TInfoDeficiencia; pTipo: Integer = 0);
begin
  if (pInfoDeficiencia.DefFisica = tpSim) or (pInfoDeficiencia.DefVisual = tpSim) or
     (pInfoDeficiencia.DefAuditiva = tpSim) or (pInfoDeficiencia.DefMental = tpSim) or
     (pInfoDeficiencia.DefIntelectual = tpSim) or (pInfoDeficiencia.ReabReadap = tpSim) then
  begin
    Gerador.wGrupo('infoDeficiencia');

    Gerador.wCampo(tcStr, '', 'defFisica',      1, 1, 1, eSSimNaoToStr(pInfoDeficiencia.DefFisica));
    Gerador.wCampo(tcStr, '', 'defVisual',      1, 1, 1, eSSimNaoToStr(pInfoDeficiencia.DefVisual));
    Gerador.wCampo(tcStr, '', 'defAuditiva',    1, 1, 1, eSSimNaoToStr(pInfoDeficiencia.DefAuditiva));
    Gerador.wCampo(tcStr, '', 'defMental',      1, 1, 1, eSSimNaoToStr(pInfoDeficiencia.DefMental));
    Gerador.wCampo(tcStr, '', 'defIntelectual', 1, 1, 1, eSSimNaoToStr(pInfoDeficiencia.DefIntelectual));
    Gerador.wCampo(tcStr, '', 'reabReadap',     1, 1, 1, eSSimNaoToStr(pInfoDeficiencia.reabReadap));

    if (pTipo <> 3) and (pInfoDeficiencia.infoCota <> snfNada) then
      Gerador.wCampo(tcStr, '', 'infoCota', 1, 1, 1, eSSimNaoFacultativoToStr(pInfoDeficiencia.infoCota));

    Gerador.wCampo(tcStr, '', 'observacao', 1, 255, 0, pInfoDeficiencia.Observacao);

    Gerador.wGrupo('/infoDeficiencia');
  end;
end;

procedure TeSocialEvento.GerarInfoDecJud(pInfoDecJud: TInfoDecJud);
begin
  if (pInfoDecJud.nrProcJud <> EmptyStr) then
  begin
    Gerador.wGrupo('infoDecJud');

    Gerador.wCampo(tcStr, '', 'nrProcJud', 1, 20, 1, pInfoDecJud.nrProcJud);

   Gerador.wGrupo('/infoDecJud');
  end;
end;

procedure TeSocialEvento.GerarInfoEstatutario(
  pInfoEstatutario: TInfoEstatutario);
begin
  if ((pInfoEstatutario.dtNomeacao > 0) and (VersaoDF <= ve02_05_00)) or
     ((pInfoEstatutario.dtExercicio > 0) and (VersaoDF >= ve02_05_00)) then
  begin
    Gerador.wGrupo('infoEstatutario');

    if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcStr, '', 'indProvim',   1,  1, 1, eSIndProvimToStr(pInfoEstatutario.indProvim));

    Gerador.wCampo(tcStr, '', 'tpProv',      1,  2, 1, eSTpProvToStr(pInfoEstatutario.tpProv));

    if VersaoDF <= ve02_05_00 then
    begin
      Gerador.wCampo(tcDat, '', 'dtNomeacao', 10, 10, 1, pInfoEstatutario.dtNomeacao);
      Gerador.wCampo(tcDat, '', 'dtPosse',    10, 10, 0, pInfoEstatutario.dtPosse);

      if ((eSIndProvimToStr(pInfoEstatutario.indProvim) = '1') or
          (eSIndProvimToStr(pInfoEstatutario.indProvim) = '2') or
          (DateToStr(pInfoEstatutario.dtExercicio) <> '')) then
        Gerador.wCampo(tcDat, '', 'dtExercicio', 10, 10, 0, pInfoEstatutario.dtExercicio);
    end
    else
      Gerador.wCampo(tcDat, '', 'dtExercicio', 10, 10, 1, pInfoEstatutario.dtExercicio);

    if pInfoEstatutario.tpPlanRP <> prpNenhum then
      Gerador.wCampo(tcStr, '', 'tpPlanRP', 0, 1, 0,  eSTpPlanRPToStr(pInfoEstatutario.tpPlanRP));

    if VersaoDF <= ve02_05_00 then
      GerarInfoDecJud(pInfoEstatutario.infoDecJud)
    else
    begin
      if pInfoEstatutario.indTetoRGPS <> snfNada then
        Gerador.wCampo(tcStr, '', 'indTetoRGPS', 0, 1, 0,  eSSimNaoFacultativoToStr(pInfoEstatutario.indTetoRGPS));

      if pInfoEstatutario.indAbonoPerm <> snfNada then
        Gerador.wCampo(tcStr, '', 'indAbonoPerm', 0, 1, 0,  eSSimNaoFacultativoToStr(pInfoEstatutario.indAbonoPerm));

      if (pInfoEstatutario.indAbonoPerm <> snfNada) and
         (pInfoEstatutario.dtIniAbono > 0) then
        Gerador.wCampo(tcDat, '', 'dtIniAbono', 10, 10, 1, pInfoEstatutario.dtIniAbono);
    end;

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
  i: Integer;
begin
  for i := 0 to objItensRemun.Count - 1 do
  begin
    Gerador.wGrupo(GroupName);

    Gerador.wCampo(tcStr, '', 'codRubr',    1, 30, 1, objItensRemun.Items[i].codRubr);
    Gerador.wCampo(tcStr, '', 'ideTabRubr', 1,  8, 1, objItensRemun.Items[i].ideTabRubr);
    Gerador.wCampo(tcDe2, '', 'qtdRubr',    1,  6, 0, objItensRemun.Items[i].qtdRubr);
    Gerador.wCampo(tcDe2, '', 'fatorRubr',  1,  5, 0, objItensRemun.Items[i].fatorRubr);

    if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcDe2, '', 'vrUnit',     1, 14, 0, objItensRemun.Items[i].vrUnit);

    Gerador.wCampo(tcDe2, '', 'vrRubr',     1, 14, 1, objItensRemun.Items[i].vrRubr);

    if VersaoDF > ve02_05_00 then
      if objItensRemun.items[i].indApurIR > tiaiNenhum then
        Gerador.wCampo(tcStr, '', 'indApurIR',  1,  1, 0, eSTpindApurIRToStr(objItensRemun.Items[i].indApurIR));

    Gerador.wGrupo('/' + GroupName);
  end;

  if objItensRemun.Count > 200 then
    Gerador.wAlerta('', GroupName, 'Lista de ' + GroupName, ERR_MSG_MAIOR_MAXIMO + '200');
end;

procedure TeSocialEvento.GerarLocalTrabalho(pLocalTrabalho: TLocalTrabalho);
begin
  Gerador.wGrupo('localTrabalho');

  GerarLocalTrabGeral(pLocalTrabalho.LocalTrabGeral);

  if VersaoDF <= ve02_05_00 then
    GerarLocalTrabDom(pLocalTrabalho.LocalTrabDom)
  else
    GerarLocalTempDom(pLocalTrabalho.LocalTempDom);
  Gerador.wGrupo('/localTrabalho');
end;

procedure TeSocialEvento.GerarLocalTrabDom(pLocalTrabDom: TLocalTrabDom);
begin
  if NaoEstaVazio(pLocalTrabDom.TpLograd) then
  begin
    Gerador.wGrupo('localTrabDom');

    Gerador.wCampo(tcStr, '', 'tpLograd',    1,  4, 1, pLocalTrabDom.TpLograd);
    Gerador.wCampo(tcStr, '', 'dscLograd',   1, 80, 1, pLocalTrabDom.DscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd',    1, 10, 1, pLocalTrabDom.NrLograd);
    Gerador.wCampo(tcStr, '', 'complemento', 0, 30, 0, pLocalTrabDom.Complemento);
    Gerador.wCampo(tcStr, '', 'bairro',      0, 60, 0, pLocalTrabDom.Bairro);
    Gerador.wCampo(tcStr, '', 'cep',         1,  8, 1, pLocalTrabDom.Cep);
    Gerador.wCampo(tcInt, '', 'codMunic',    7,  7, 1, pLocalTrabDom.CodMunic);
    Gerador.wCampo(tcStr, '', 'uf',          2,  2, 1, pLocalTrabDom.Uf);

    Gerador.wGrupo('/localTrabDom');
  end;
end;

procedure TeSocialEvento.GerarLocalTempDom(pLocalTempDom: TLocalTempDom);
begin
  if NaoEstaVazio(pLocalTempDom.TpLograd) then
  begin
    Gerador.wGrupo('localTempDom');

    Gerador.wCampo(tcStr, '', 'tpLograd',    1,  4, 1, pLocalTempDom.TpLograd);
    Gerador.wCampo(tcStr, '', 'dscLograd',   1, 80, 1, pLocalTempDom.DscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd',    1, 10, 1, pLocalTempDom.NrLograd);
    Gerador.wCampo(tcStr, '', 'complemento', 0, 30, 0, pLocalTempDom.Complemento);
    Gerador.wCampo(tcStr, '', 'bairro',      0, 60, 0, pLocalTempDom.Bairro);
    Gerador.wCampo(tcStr, '', 'cep',         1,  8, 1, pLocalTempDom.Cep);
    Gerador.wCampo(tcInt, '', 'codMunic',    7,  7, 1, pLocalTempDom.CodMunic);
    Gerador.wCampo(tcStr, '', 'uf',          2,  2, 1, pLocalTempDom.Uf);

    Gerador.wGrupo('/localTempDom');
  end;
end;

procedure TeSocialEvento.GerarLocalTrabGeral(pLocalTrabGeral: TLocalTrabGeral);
begin
  if NaoEstaVazio(pLocalTrabGeral.NrInsc) then
  begin
    Gerador.wGrupo('localTrabGeral');

    Gerador.wCampo(tcStr, '', 'tpInsc',   1,  1, 1, eSTpInscricaoToStr(pLocalTrabGeral.TpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc',   1, 15, 1, pLocalTrabGeral.NrInsc);
    Gerador.wCampo(tcStr, '', 'descComp', 0, 80, 0, pLocalTrabGeral.DescComp);

    Gerador.wGrupo('/localTrabGeral');
  end;
end;

procedure TeSocialEvento.GerarModoAbertura(pModo: TModoLancamento);
begin
  case pModo of
    mlAlteracao: Gerador.wGrupo('alteracao');
    mlExclusao: Gerador.wGrupo('exclusao');
  else
    Gerador.wGrupo('inclusao');
  end;
end;

procedure TeSocialEvento.GerarModoFechamento(pModo: TModoLancamento);
begin
  case pModo of
    mlAlteracao: Gerador.wGrupo('/alteracao');
    mlExclusao: Gerador.wGrupo('/exclusao');
  else
    Gerador.wGrupo('/inclusao');
  end;
end;

procedure TeSocialEvento.GerarNascimento(pNasc: TNascimento; const pGroupName: string = 'nascimento');
begin
  Gerador.wGrupo(pGroupName);

  Gerador.wCampo(tcDat, '', 'dtNascto',  10, 10, 1, pNasc.DtNascto);

  if VersaoDF <= ve02_05_00 then
  begin
    Gerador.wCampo(tcInt, '', 'codMunic',   1,  7, 0, pNasc.CodMunic);
    Gerador.wCampo(tcStr, '', 'uf',         2,  2, 0, pNasc.UF);
  end;

  Gerador.wCampo(tcStr, '', 'paisNascto', 1,  3, 1, pNasc.PaisNascto);
  Gerador.wCampo(tcStr, '', 'paisNac',    1,  3, 1, pNasc.PaisNac);

  if VersaoDF <= ve02_05_00 then
  begin
    Gerador.wCampo(tcStr, '', 'nmMae',      1, 70, 0, pNasc.NmMae);
    Gerador.wCampo(tcStr, '', 'nmPai',      1, 70, 0, pNasc.NmPai);
  end;

  Gerador.wGrupo('/' + pGroupName);
end;

procedure TeSocialEvento.GerarOC(pOc: TOC);
begin
  if NaoEstaVazio(pOc.NrOc) then
  begin
    Gerador.wGrupo('OC');

    Gerador.wCampo(tcStr, '', 'nrOc',          1, 14, 1, pOc.NrOc);
    Gerador.wCampo(tcStr, '', 'orgaoEmissor',  1, 20, 1, pOc.OrgaoEmissor);
    Gerador.wCampo(tcDat, '', 'dtExped',      10, 10, 0, pOc.DtExped);
    Gerador.wCampo(tcDat, '', 'dtValid',      10, 10, 0, pOc.DtValid);

    Gerador.wGrupo('/OC');
  end;
end;

procedure TeSocialEvento.GerarPensaoAlim(objPensaoAlim: TPensaoAlimCollection; const GroupName: String = 'pensaoAlim');
var
  i: Integer;
begin
  for i := 0 to objPensaoAlim.Count - 1 do
  begin
    Gerador.wGrupo(GroupName);

    Gerador.wCampo(tcStr, '', 'cpfBenef',      11, 11, 1, objPensaoAlim.Items[i].cpfBenef);
    Gerador.wCampo(tcDat, '', 'dtNasctoBenef', 10, 10, 0, objPensaoAlim.Items[i].dtNasctoBenef);
    Gerador.wCampo(tcStr, '', 'nmBenefic',      1, 70, 1, objPensaoAlim.Items[i].nmBenefic);
    Gerador.wCampo(tcDe2, '', 'vlrPensao',      1, 14, 1, objPensaoAlim.Items[i].vlrPensao);

    Gerador.wGrupo('/'+GroupName);
  end;

  if objPensaoAlim.Count > 99 then
    Gerador.wAlerta('', GroupName, 'Lista de ' + GroupName, ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TeSocialEvento.GerarProcessoGenerico(const pChave: string; pProcesso: TProcesso);
begin
  Gerador.wGrupo(pChave);

  Gerador.wCampo(tcStr, '', 'nrProc',  1, 21, 1, pProcesso.nrProc);
  Gerador.wCampo(tcStr, '', 'codSusp', 1, 14, 0, pProcesso.codSusp);

  Gerador.wGrupo('/' + pChave);
end;

procedure TeSocialEvento.GerarProcessoAdmJudFap(pProcAdmJudFap: TProcAdmJudFap);
begin
  Gerador.wGrupo('procAdmJudFap');

  Gerador.wCampo(tcStr, '', 'tpProc',  1,  1, 1, eSTpProcessoToStr(pProcAdmJudFap.tpProc));
  Gerador.wCampo(tcStr, '', 'nrProc',  1, 20, 1, pProcAdmJudFap.nrProc);
  Gerador.wCampo(tcStr, '', 'codSusp', 1, 14, 0, pProcAdmJudFap.codSusp);

  Gerador.wGrupo('/procAdmJudFap');
end;

procedure TeSocialEvento.GerarProcessoAdmJudRat(pProcAdmJudRat: TProcAdmJudRat);
begin
  Gerador.wGrupo('procAdmJudRat');

  Gerador.wCampo(tcStr, '', 'tpProc',  1,  1, 1, eSTpProcessoToStr(pProcAdmJudRat.tpProc));
  Gerador.wCampo(tcStr, '', 'nrProc',  1, 20, 1, pProcAdmJudRat.nrProc);
  Gerador.wCampo(tcStr, '', 'codSusp', 1, 14, 0, pProcAdmJudRat.codSusp);

  Gerador.wGrupo('/procAdmJudRat');
end;

procedure TeSocialEvento.GerarProcJudTrab(objProcJudTrab: TProcJudTrabCollection;
  tpTrib: Boolean);
var
  i: Integer;
begin
  for i := 0 to objProcJudTrab.Count - 1 do
  begin
    Gerador.wGrupo('procJudTrab');

    if tpTrib then
      Gerador.wCampo(tcStr, '', 'tpTrib', 1, 1, 1, eSTpTributoToStr(objProcJudTrab.Items[i].tpTrib));

    Gerador.wCampo(tcStr, '', 'nrProcJud', 1, 20, 1, objProcJudTrab.Items[i].nrProcJud);
    Gerador.wCampo(tcStr, '', 'codSusp',   1, 14, 1, objProcJudTrab.Items[i].codSusp);

    Gerador.wGrupo('/procJudTrab');
  end;

  if objProcJudTrab.Count > 99 then
    Gerador.wAlerta('', 'procJudTrab', 'Lista de Processos Judiciais', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TeSocialEvento.GerarIdeTrabalhador(
  pideTrabalhador: TideTrabalhador; const GeraGrupo: boolean);
begin
  if GeraGrupo then
    Gerador.wGrupo('ideTrabalhador');

  Gerador.wCampo(tcStr, '', 'cpfTrab', 11, 11, 1, pideTrabalhador.CpfTrab);

  if GeraGrupo then
    Gerador.wGrupo('/ideTrabalhador');
end;

procedure TeSocialEvento.GerarIdeTrabalhador2(
  pideTrabalhador: TideTrabalhador2; const GeraGrupo: boolean);
begin
  if GeraGrupo then
    Gerador.wGrupo('ideTrabalhador');

  GerarIdeTrabalhador(pideTrabalhador, False);

  if VersaoDF <= ve02_05_00 then
    Gerador.wCampo(tcStr, '', 'nisTrab', 1, 11, 0, pideTrabalhador.nisTrab);

  if GeraGrupo then
    Gerador.wGrupo('/ideTrabalhador');
end;

procedure TeSocialEvento.GerarIdeFolhaPagto(
  pIdeFolhaPagto: TIdeFolhaPagto);
begin
  if pIdeFolhaPagto.perApur <> EmptyStr then
  begin
    Gerador.wGrupo('ideFolhaPagto');

    Gerador.wCampo(tcStr, '', 'indApuracao', 1, 1, 1, eSIndApuracaoToStr(pIdeFolhaPagto.indApuracao));
    Gerador.wCampo(tcStr, '', 'perApur',     7, 7, 1, pIdeFolhaPagto.perApur);

    Gerador.wGrupo('/ideFolhaPagto');
  end;
end;

procedure TeSocialEvento.GerarIdeFolhaPagto2(pIdeFolhaPagto: TIdeFolhaPagto);
begin
  if pIdeFolhaPagto.perApur <> EmptyStr then
  begin
    Gerador.wGrupo('ideFolhaPagto');

    Gerador.wCampo(tcStr, '', 'perApur',     7, 7, 1, pIdeFolhaPagto.perApur);

    Gerador.wGrupo('/ideFolhaPagto');
  end;

end;

procedure TeSocialEvento.GerarEmitente(pEmitente: TEmitente; ATipoEvento: TTipoEvento);
begin
  Gerador.wGrupo('emitente');

  Gerador.wCampo(tcStr, '', 'nmEmit', 1, 70, 1, pEmitente.nmEmit);
  Gerador.wCampo(tcStr, '', 'ideOC',  1,  1, 1, eSIdeOCToStrEX(pEmitente.ideOC));

  case ATipoEvento of
    teS2210: Gerador.wCampo(tcStr, '', 'nrOC', 1, 14, 1, pEmitente.nrOc);
    teS2230: Gerador.wCampo(tcStr, '', 'nrOc', 1, 14, 1, pEmitente.nrOc);
  end;

  Gerador.wCampo(tcStr, '', 'ufOC', 2, 2, 0, pEmitente.ufOC);

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
  i: Integer;
begin
  for i := 0 to objDetOper.Count - 1 do
  begin
    Gerador.wGrupo('detOper');

    Gerador.wCampo(tcStr, '', 'cnpjOper', 14, 14, 1, objDetOper.Items[i].cnpjOper);
    Gerador.wCampo(tcStr, '', 'regANS',    1,  6, 1, objDetOper.Items[i].regANS);
    Gerador.wCampo(tcDe2, '', 'vrPgTit',   1, 14, 1, objDetOper.Items[i].vrPgTit);

    GerarDetPlano(objDetOper.Items[i].detPlano);

    Gerador.wGrupo('/detOper');
  end;

  if objDetOper.Count > 99 then
    Gerador.wAlerta('', 'detOper', 'Lista de Detalhamento de valores', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TeSocialEvento.GerarDetPlano(objDetPlanoCollection: TDetPlanoCollection);
var
  i: Integer;
begin
  for i := 0 to objDetPlanoCollection.Count - 1 do
  begin

    Gerador.wGrupo('detPlano');

    Gerador.wCampo(tcStr, '', 'tpDep',     1,  2, 1, eStpDepToStr(objDetPlanoCollection.Items[i].tpDep));
    Gerador.wCampo(tcStr, '', 'cpfDep',   11, 11, 0, objDetPlanoCollection.Items[i].cpfDep);
    Gerador.wCampo(tcStr, '', 'nmDep',     1, 70, 1, objDetPlanoCollection.Items[i].nmDep);
    Gerador.wCampo(tcDat, '', 'dtNascto', 10, 10, 1, objDetPlanoCollection.Items[i].dtNascto);
    Gerador.wCampo(tcDe2, '', 'vlrPgDep',  1, 14, 1, objDetPlanoCollection.Items[i].vlrPgDep);

    Gerador.wGrupo('/detPlano');
  end;

  if objDetPlanoCollection.Count > 99 then
    Gerador.wAlerta('', 'detPlano', 'Lista de Informações do Dependentes', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TeSocialEvento.GerarNfs(pNfs: TNfsColecao);
var
  i: integer;
begin
  for i := 0 to pNfs.Count - 1 do
  begin
    Gerador.wGrupo('nfs');

    if VersaoDF < ve02_05_00 then
    begin
      Gerador.wCampo(tcStr, '', 'serie',        1,  5, 0, pNfs.Items[i].serie);
      Gerador.wCampo(tcStr, '', 'nrDocto',      1, 20, 1, pNfs.Items[i].nrDocto);
      Gerador.wCampo(tcDat, '', 'dtEmisNF',    10, 10, 1, pNfs.Items[i].dtEmisNF);
      Gerador.wCampo(tcDe2, '', 'vlrBruto',     1, 14, 1, pNfs.Items[i].vlrBruto);
      Gerador.wCampo(tcDe2, '', 'vrCPDescPR',   1, 14, 1, pNfs.Items[i].vrCPDescPR);
      Gerador.wCampo(tcDe2, '', 'vrRatDescPR',  1, 14, 1, pNfs.Items[i].vrRatDescPR);
      Gerador.wCampo(tcDe2, '', 'vrSenarDesc',  1, 14, 1, pNfs.Items[i].vrSenarDesc);
    end
    else if VersaoDF >= ve02_05_00 then
    begin
      Gerador.wGrupo('nfs serie="' + pNfs.Items[i].serie + '"' +
                        ' nrDocto="' + pNfs.Items[i].nrDocto + '"' +
                        ' dtEmisNF="' + FormatDateTime('yyyy-mm-dd', pNfs.Items[i].dtEmisNF) + '"' +
                        ' vlrBruto="' + FloatToString(pNfs.Items[i].vlrBruto, '.', FloatMask(2, False)) + '"' +
                        ' vrCPDescPR="' + FloatToString(pNfs.Items[i].vrCPDescPR, '.', FloatMask(2, False)) + '"' +
                        ' vrRatDescPR="' + FloatToString(pNfs.Items[i].vrRatDescPR, '.', FloatMask(2, False)) + '"' +
                        ' vrSenarDesc="' + FloatToString(pNfs.Items[i].vrSenarDesc, '.', FloatMask(2, False)) + '"');
    end;

    Gerador.wGrupo('/nfs');
  end;

  if pNfs.Count > 9999 then
    Gerador.wAlerta('', 'nfs', 'Lista de Notas Fiscais', ERR_MSG_MAIOR_MAXIMO + '9999');
end;

procedure TeSocialEvento.GerarRemunOutrEmpr(objRemunOutrEmpr: TRemunOutrEmprCollection);
var
  i: integer;
begin
  for i := 0 to objRemunOutrEmpr.Count - 1 do
  begin
    if objRemunOutrEmpr.Items[i].vlrRemunOE > 0 then
    begin
      Gerador.wGrupo('remunOutrEmpr');

      Gerador.wCampo(tcStr, '', 'tpInsc',     1,  1, 1, eSTpInscricaoToStr(objRemunOutrEmpr.Items[i].tpInsc));
      Gerador.wCampo(tcStr, '', 'nrInsc',     1, 15, 1, objRemunOutrEmpr.Items[i].nrInsc);
      Gerador.wCampo(tcInt, '', 'codCateg',   1,  3, 1, objRemunOutrEmpr.Items[i].codCateg);
      Gerador.wCampo(tcDe2, '', 'vlrRemunOE', 1, 14, 1, objRemunOutrEmpr.Items[i].vlrRemunOE);

      Gerador.wGrupo('/remunOutrEmpr');
    end;
  end;

  if objRemunOutrEmpr.Count > 10 then
    Gerador.wAlerta('', 'remunOutrEmpr', 'Lista de Informações de Remuneração', ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TeSocialEvento.GerarInfoMV(pInfoMV: TInfoMV);
begin
  if pInfoMV.indMV in [imvDescontadaempregador, imvDescontadaoutras, imvSobrelimite] then
  begin
    Gerador.wGrupo('infoMV');

    Gerador.wCampo(tcStr, '', 'indMV', 1, 1, 1, eSIndMVToStr(pInfoMV.indMV));

    GerarRemunOutrEmpr(pInfoMV.remunOutrEmpr);

    Gerador.wGrupo('/infoMV');
  end;
end;

procedure TeSocialEvento.GerarInfoSimples(obj: TinfoSimples);
begin
  if obj.indSimples <> idsNenhum then
  begin
    Gerador.wGrupo('infoSimples');

    Gerador.wCampo(tcStr, '', 'indSimples', 1, 1, 1, eSIndSimplesToStr(obj.indSimples));

    Gerador.wGrupo('/infoSimples');
  end;
end;

procedure TeSocialEvento.GerarIdeEstabLot(pIdeEstabLot: TideEstabLotCollection);
var
  i: integer;
begin
  for i := 0 to pIdeEstabLot.Count - 1 do
  begin
    Gerador.wGrupo('ideEstabLot');

    Gerador.wCampo(tcInt, '', 'tpInsc',     1,  1, 1, eSTpInscricaoToStr(pIdeEstabLot[i].tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc',     1, 15, 1, pIdeEstabLot[i].nrInsc);
    Gerador.wCampo(tcStr, '', 'codLotacao', 1, 30, 1, pIdeEstabLot[i].codLotacao);

    GerarItensRemun(pIdeEstabLot[i].detVerbas, 'detVerbas');

    if (VersaoDF <= ve02_05_00) and (pIdeEstabLot[i].infoSaudeColetInst) then
      GerarInfoSaudeColet(pIdeEstabLot[i].infoSaudeColet);

    if pIdeEstabLot[i].infoAgNocivoInst then
      GerarInfoAgNocivo(pIdeEstabLot[i].infoAgNocivo);

    if pIdeEstabLot[i].infoSimplesInst then
      GerarInfoSimples(pIdeEstabLot[i].infoSimples);

    Gerador.wGrupo('/ideEstabLot');
  end;

  if pIdeEstabLot.Count > 24 then
    Gerador.wAlerta('', 'ideEstabLot', 'Lista de Identificação de Estabecimentos/Lotação', ERR_MSG_MAIOR_MAXIMO + '24');
end;

procedure TeSocialEvento.GerarQuarentena(obj: TQuarentena);
begin
  if VersaoDF <= veS01_00_00 then
  begin
    if obj.dtFimQuar > 0 then
    begin
      Gerador.wGrupo('quarentena');

      Gerador.wCampo(tcDat, '', 'dtFimQuar', 10, 10, 1, obj.dtFimQuar);

      Gerador.wGrupo('/quarentena');
    end;
  end;
end;

procedure TeSocialEvento.GerarIdeRespInf(obj: TIdeRespInf);
begin
  Gerador.wGrupo('ideRespInf');

  Gerador.wCampo(tcStr, '', 'nmResp',    1, 70, 1, obj.nmResp);
  Gerador.wCampo(tcStr, '', 'cpfResp',  11, 11, 1, obj.cpfResp);
  Gerador.wCampo(tcStr, '', 'telefone',  1, 13, 1, obj.telefone);
  Gerador.wCampo(tcStr, '', 'email',     1, 60, 0, obj.email);

  Gerador.wGrupo('/ideRespInf');
end;

procedure TeSocialEvento.GerarObservacoes(
  pObservacoes: TObservacoesCollection);
var
  i: integer;
begin
  for i := 0 to pObservacoes.Count - 1 do
  begin
    Gerador.wGrupo('observacoes');

    Gerador.wCampo(tcStr, '', 'observacao', 1, 255, 1, pObservacoes.Items[i].observacao);

    Gerador.wGrupo('/observacoes');
  end;

  if pObservacoes.Count > 99 then
    Gerador.wAlerta('', 'observacoes', 'Lista de Observações', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TeSocialEvento.GerarTransfDom(pTransfDom: TTransfDom);
begin
  if pTransfDom.cpfSubstituido = '' then
    Exit;

  Gerador.wGrupo('transfDom');

  Gerador.wCampo(tcStr, '', 'cpfSubstituido', 11, 11, 1, pTransfDom.cpfSubstituido);
  Gerador.wCampo(tcStr, '', 'matricAnt',       0, 30, 0, pTransfDom.matricAnt);
  Gerador.wCampo(tcDat, '', 'dtTransf',       10, 10, 1, pTransfDom.dtTransf);

  Gerador.wGrupo('/transfDom');
end;

procedure TeSocialEvento.GerarEnderecoLocalTrabInterm(pEndereco: TBrasil);
begin
  if NaoEstaVazio(pEndereco.TpLograd) then
  begin
    Gerador.wGrupo('localTrabInterm');

    Gerador.wCampo(tcStr, '', 'tpLograd',  1,  4, 1, pEndereco.TpLograd);
    Gerador.wCampo(tcStr, '', 'dscLograd', 1, 80, 1, pEndereco.DscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd',  1, 10, 1, pEndereco.NrLograd);
    Gerador.wCampo(tcStr, '', 'complem',   0, 30, 0, pEndereco.Complemento);
    Gerador.wCampo(tcStr, '', 'bairro',    0, 60, 0, pEndereco.Bairro);
    Gerador.wCampo(tcStr, '', 'cep',       1,  8, 1, pEndereco.Cep);
    Gerador.wCampo(tcInt, '', 'codMunic',  7,  7, 1, pEndereco.CodMunic);
    Gerador.wCampo(tcStr, '', 'uf',        2,  2, 1, pEndereco.Uf);

    Gerador.wGrupo('/localTrabInterm');
  end;
end;

procedure TeSocialEvento.GerarTreinamentoCapacitacao(objTreiCap: TtreiCapCollection);
var
  i: Integer;
begin
  for i := 0 to objTreiCap.Count - 1 do
  begin
    if (objTreiCap.Items[i].codTreiCap = 1006) or (objTreiCap.Items[i].codTreiCap = 1207) or
       ((objTreiCap.Items[i].codTreiCap >= 3701) and (objTreiCap.Items[i].codTreiCap <= 3719)) then
    begin
      Gerador.wGrupo('treiCap');

      Gerador.wCampo(tcInt, '', 'codTreiCap',  4,  4, 1, objTreiCap.Items[i].codTreiCap);

      Gerador.wGrupo('/treiCap');
    end;
  end;

  if objTreiCap.Count > 99 then
    Gerador.wAlerta('', 'treiCap', 'Treinamentos, capacitações, exercícios simulados, autorizações ou outras anotações', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TeSocialEvento.GerarCessao(Obj: Tcessao);
begin
  if Obj.dtIniCessao > 0 then
  begin
    Gerador.wGrupo('cessao');

    Gerador.wCampo(tcDat, '', 'dtIniCessao',  10, 10, 1, obj.dtIniCessao);

    Gerador.wGrupo('/cessao');
  end;
end;

function TeSocialEvento.GerarXML: boolean;
begin
  Result := False;
  with TACBreSocial(FACBreSocial).Configuracoes.Geral do
  begin
    Self.VersaoDF := VersaoDF;
    Self.Gerador.Opcoes.FormatoAlerta := FormatoAlerta;
    Self.Gerador.Opcoes.RetirarAcentos := RetirarAcentos;
    Self.Gerador.Opcoes.RetirarEspacos := RetirarEspacos;
    Self.Gerador.Opcoes.IdentarXML := IdentarXML;
  end;
end;

procedure TeSocialEvento.GerarIdeAdv(obj: TIdeAdvCollection);
var
  i: integer;
begin
  for i := 0 to obj.Count - 1 do
  begin
    Gerador.wGrupo('ideAdv');

    Gerador.wCampo(tcStr, '', 'tpInsc',         1,  1, 1, eSTpInscricaoToStr(obj.Items[i].tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc',        14, 14, 1, obj.Items[i].nrInsc);
    Gerador.wCampo(tcDe2, '', 'vlrAdv',         1, 14, 1, obj.Items[i].vlrAdv);

    Gerador.wGrupo('/ideAdv');
  end;

  if obj.Count > 99 then
    Gerador.wAlerta('', 'ideAdv', 'Identificação dos advogados', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TeSocialEvento.GerarDespProcJud(obj: TDespProcJud);
begin
  Gerador.wGrupo('despProcJud');

  Gerador.wCampo(tcDe2, '', 'vlrDespCustas',    0, 14, 0, obj.vlrDespCustas);
  Gerador.wCampo(tcDe2, '', 'vlrDespAdvogados', 0, 14, 0, obj.vlrDespAdvogados);

  Gerador.wGrupo('/despProcJud');
end;

procedure TeSocialEvento.GerarInfoRRA(obj: TInfoRRA);
begin
  Gerador.wGrupo('infoRRA');

  Gerador.wCampo(tcStr, '', 'tpProcRRA',        1,  1, 1, eSTpProcRRAToStr(obj.tpProcRRA));
  Gerador.wCampo(tcStr, '', 'nrProcRRA',        0,  1, 0, obj.nrProcRRA);
  Gerador.wCampo(tcStr, '', 'descRRA',          1, 50, 1, obj.descRRA);
  Gerador.wCampo(tcDe1, '', 'qtdMesesRRA',      1,  4, 1, obj.qtdMesesRRA);

  if obj.instDespProcJud() then
    GerarDespProcJud(obj.despProcJud);

  if obj.instIdeAdv() then
    GerarIdeAdv(obj.ideAdv);

  Gerador.wGrupo('/infoRRA');
end;

end.
