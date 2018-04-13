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

unit pcesGerador;

interface

uses
  SysUtils, Classes, StrUtils, variants,
  ACBrUtil,
  pcnGerador, pcnConversao, pcnAuxiliar,
  pcesCommon, pcesConversaoeSocial;

type
  TGeradorOpcoes = class;
  TeSocialEvento = class;

  TeSocialEvento = class(TeSocial)
  private
    FACBreSocial: TObject; //alimenta no create
    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FVersaoDF: TVersaoeSocial;

    FGerador: TGerador;
    FSchema: TeSocialSchema;
    FXML: AnsiString;
  public
    constructor Create(AACBreSocial: TObject); overload;//->recebe a instancia da classe TACBreSocial
    destructor Destroy; override;

    function  GerarXML: boolean; virtual; abstract;
    procedure SaveToFile(const CaminhoArquivo: string);
    function  Assinar(XMLEvento: String; NomeEvento: String): AnsiString;
    function  GerarChaveEsocial(const emissao: TDateTime;
                                const CNPJF: string;
                                sequencial: Integer): String;
    procedure Validar(Schema: TeSocialSchema);

    property Alertas: String read FAlertas;
    property ErroValidacao: String read FErroValidacao;
    property ErroValidacaoCompleto: String read FErroValidacaoCompleto;
    property VersaoDF: TVersaoeSocial read FVersaoDF write FVersaoDF;
  protected
    {Geradores de Uso Comum}
    procedure GerarCabecalho(Namespace: String);
    procedure GerarRodape;
    procedure GerarAliqGilRat(pEmp: TIdeEmpregador; pAliqRat: TAliqGilRat; const GroupName: string = 'aliqGilRat');
    procedure GerarAlvaraJudicial(pAlvaraJudicial: TAlvaraJudicial);
    procedure GerarAposentadoria(pAposentadoria: TAposentadoria);
    procedure GerarCNH(pCnh: TCNH);                                   
    procedure GerarContatoTrabalhador(pContato: TContatoTrabalhador);
    procedure GerarInfoContrato(pInfoContrato: TInfoContrato; pTipo: Integer; pInfoRegimeTrab: TInfoRegimeTrab);
    procedure GerarObservacoes(pObservacoes: TObservacoesCollection);
    procedure GerarTransfDom(pTransfDom: TTransfDom);
    procedure GerarCTPS(pCTPS: TCTPS);
    procedure GerarDependente(pDependente: TDependenteCollection);
    procedure GerarDescAtividade(pDescAtividade: TDescAtividadeCollection);
    procedure GerarDocumentos(pDocumentos: TDocumentos);
    procedure GerarDuracao(pDuracao: TDuracao; pTipo: Integer);
    procedure GerarEndereco(pEndereco: TEndereco; pExterior: boolean = false);
    procedure GerarEnderecoBrasil(pEndereco: TBrasil; const GroupName: string = 'brasil');
    procedure GerarEnderecoExterior(pEndereco: TExterior);
    procedure GerarEnderecoLocalTrabInterm(pEndereco: TBrasil);
    procedure GerarEpi(pEpi: TEpiCollection);
    procedure GerarFGTS(pFgts: TFGTS);
    procedure GerarFiliacaoSindical(pFiliacaoSindical: TFiliacaoSindical);
    procedure GerarHorario(pHorario: THorarioCollection);
    procedure GerarHorContratual(pHorContratual: THorContratual);
    procedure GerarIdeEvento(pEvt: TIdeEvento; const GeraGrupo: boolean = True);
    procedure GerarIdeEvento2(pEvt: TIdeEvento2; const GeraGrupo: boolean = True; GeraRetif: Boolean = True);
    procedure GerarIdeEvento3(pEvt: TIdeEvento3; IndRetif: Boolean=True);
    procedure GerarIdeEvento4(pEvt: TIdeEvento4);
    procedure GerarIdeEvento5(pEvt: TIdeEvento5; nrRecArqBase: Boolean = True; IndApuracao: Boolean = True);
    procedure GerarIdePeriodo(pIdePeriodo: TidePeriodo; const GroupName: string = 'idePeriodo');
    procedure GerarIdeEmpregador(pEmp: TIdeEmpregador);
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
    procedure GerarIdeTrabalhador(pideTrabalhador: TideTrabalhador; const GeraGrupo: boolean = True);
    procedure GerarIdeTrabalhador2(pideTrabalhador: TideTrabalhador2; const GeraGrupo: boolean);
    procedure GerarIdeTrabalhador3(pideTrabalhador: TideTrabalhador3; const tpTrib: boolean = True);
    procedure GerarIdeFolhaPagto(pIdeFolhaPagto: TIdeFolhaPagto);
    procedure GerarEmitente(pEmitente: TEmitente);
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
  ACBreSocial, ACBreSocialEventos, ACBrDFeSSL;

{TeSocialEvento}

function TeSocialEvento.Assinar(XMLEvento, NomeEvento: String): AnsiString;
var
  XMLAss, ArqXML: string;
begin
  Result := '';

  ArqXML := XMLEvento;

  // XML já deve estar em UTF8, para poder ser assinado //
  ArqXML := ConverteXMLtoUTF8(ArqXML);
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

    NomeEvento := NomeEvento + '.xml';

//    if Configuracoes.Arquivos.Salvar then
//      Gravar(NomeEvento, XMLAss, Configuracoes.Arquivos.PathSalvar);

    Result := AnsiString(XMLAss);

    {$IFDEF DEBUG}
    if Configuracoes.Arquivos.Salvar then
    begin
      With TStringList.Create do
      try
        Text := XMLAss;
        SaveToFile(IncludeTrailingPathDelimiter(Configuracoes.Arquivos.PathSalvar) + NomeEvento);
      finally
        Free;
      end;
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
    lStr.Text := string(XML);
    lStr.SaveToFile(ChangeFileExt(lFileName,'.xml'));
  finally
    lStr.Free;
  end;
end;

procedure TeSocialEvento.Validar(Schema: TeSocialSchema);
var
  Erro, AXML: String;
  EhValido: Boolean;
  Evento: string;
begin
  AXML := FXMLAssinado;
  Evento := SchemaESocialToStr(Schema);

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

procedure TeSocialEvento.GerarCabecalho(Namespace: String);
begin
  with TACBreSocial(FACBreSocial) do
  begin
    SSL.NameSpaceURI := ACBRESOCIAL_NAMESPACE_URI + Namespace + '/v' +
                        VersaoeSocialToStr(Configuracoes.Geral.VersaoDF);

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

  if (Length(CNPJF) = 14) then
    Result := Result + IntToStr(1)
  else
    Result := Result + IntToStr(2);

  if TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador in [teOrgaoPublico, tePessoaFisica] then
    Result := Result + copy(OnlyNumber(CNPJF) + '00000000000000', 1, 14)
  else
    Result := Result + copy(OnlyNumber(Copy(CNPJF, 1, 8)) + '00000000000000', 1, 14);

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

    Gerador.wCampo(tcStr, '', 'nrRegCnh',      1, 12, 1, pCnh.nrRegCnh);
    Gerador.wCampo(tcDat, '', 'dtExped',      10, 10, 0, pCnh.DtExped);
    Gerador.wCampo(tcStr, '', 'ufCnh',         2,  2, 1, eSufToStr(pCnh.ufCnh));
    Gerador.wCampo(tcDat, '', 'dtValid',      10, 10, 1, pCnh.DtValid);
    Gerador.wCampo(tcDat, '', 'dtPriHab',     10, 10, 0, pCnh.dtPriHab);
    Gerador.wCampo(tcStr, '', 'categoriaCnh',  1,  2, 1, eSCnhToStr(pCnh.categoriaCnh));

    Gerador.wGrupo('/CNH');
  end;
end;

procedure TeSocialEvento.GerarContatoTrabalhador(pContato: TContatoTrabalhador);
begin
  Gerador.wGrupo('contato');

  Gerador.wCampo(tcStr, '', 'fonePrinc',     0, 13, 0, pContato.FonePrinc);
  Gerador.wCampo(tcStr, '', 'foneAlternat',  0, 13, 0, pContato.FoneAlternat);
  Gerador.wCampo(tcStr, '', 'emailPrinc',    0, 60, 0, pContato.emailPrinc);
  Gerador.wCampo(tcStr, '', 'emailAlternat', 0, 60, 0, pContato.EmailAlternat);

  Gerador.wGrupo('/contato');
end;

procedure TeSocialEvento.GerarInfoContrato(pInfoContrato: TInfoContrato; pTipo: Integer; pInfoRegimeTrab: TInfoRegimeTrab);
begin
  Gerador.wGrupo('infoContrato');

  Gerador.wCampo(tcStr, '', 'codCargo',    0, 30, 0, pInfoContrato.CodCargo);
  Gerador.wCampo(tcStr, '', 'codFuncao',   0, 30, 0, pInfoContrato.CodFuncao);
  Gerador.wCampo(tcStr, '', 'codCateg',    1,  3, 1, pInfoContrato.CodCateg);
  Gerador.wCampo(tcStr, '', 'codCarreira', 0, 30, 0, pInfoContrato.codCarreira);
  Gerador.wCampo(tcDat, '', 'dtIngrCarr',  0, 10, 0, pInfoContrato.dtIngrCarr);

  GerarRemuneracao(pInfoContrato.Remuneracao);
  GerarDuracao(pInfoContrato.Duracao, pTipo);
  GerarLocalTrabalho(pInfoContrato.LocalTrabalho);

  //Informações do Horário Contratual do Trabalhador. O preenchimento é obrigatório se {tpRegJor} = [1]
  if (pInfoRegimeTrab.InfoCeletista.TpRegJor = rjSubmetidosHorarioTrabalho) then
    GerarHorContratual(pInfoContrato.HorContratual);

  GerarFiliacaoSindical(pInfoContrato.FiliacaoSindical);
  GerarAlvaraJudicial(pInfoContrato.AlvaraJudicial);
  GerarObservacoes(pInfoContrato.observacoes);

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

procedure TeSocialEvento.GerarDependente(pDependente: TDependenteCollection);
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
    Gerador.wCampo(tcStr, '', 'depIRRF',   1,  1, 1, eSSimNaoToStr(pDependente.Items[i].DepIRRF));
    Gerador.wCampo(tcStr, '', 'depSF',     1,  1, 1, eSSimNaoToStr(pDependente.Items[i].DepSF));
    Gerador.wCampo(tcStr, '', 'incTrab',   1,  1, 1, eSSimNaoToStr(pDependente.Items[i].incTrab));

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
      Gerador.wCampo(tcStr, '', 'descAtivDesemp  ', 0, 0, 0, pDescAtividade.Items[i].DescAtivDesemp);
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
  Gerador.wGrupo('documentos');

  GerarCTPS(pDocumentos.CTPS);
  GerarRic(pDocumentos.Ric);
  GerarRG(pDocumentos.RG);
  GerarRNE(pDocumentos.RNE);
  GerarOC(pDocumentos.OC);
  GerarCNH(pDocumentos.CNH);

  Gerador.wGrupo('/documentos');
end;

procedure TeSocialEvento.GerarDuracao(pDuracao: TDuracao; pTipo: Integer);
begin
  Gerador.wGrupo('duracao');

  Gerador.wCampo(tcStr, '', 'tpContr', 1, 1, 1, eSTpContrToStr(pDuracao.TpContr));

  if (eSTpContrToStr(pDuracao.TpContr) = '2') then
  begin
    Gerador.wCampo(tcDat, '', 'dtTerm',    10, 10, 0, pDuracao.dtTerm);

    if pTipo in  [1,2] then
      Gerador.wCampo(tcStr, '', 'clauAssec',  1,  1, 0, eSSimNaoToStr(pDuracao.clauAssec));
  end;

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

procedure TeSocialEvento.GerarEnderecoBrasil(pEndereco: TBrasil; const GroupName: string);
begin
  Gerador.wGrupo(GroupName);

  Gerador.wCampo(tcStr, '', 'tpLograd',    1,  4, 1, pEndereco.TpLograd);
  Gerador.wCampo(tcStr, '', 'dscLograd',   1, 80, 1, pEndereco.DscLograd);
  Gerador.wCampo(tcStr, '', 'nrLograd',    1, 10, 1, pEndereco.NrLograd);
  Gerador.wCampo(tcStr, '', 'complemento', 1, 30, 0, pEndereco.Complemento);
  Gerador.wCampo(tcStr, '', 'bairro',      1, 60, 0, pEndereco.Bairro);
  Gerador.wCampo(tcStr, '', 'cep',         1,  8, 1, pEndereco.Cep);
  Gerador.wCampo(tcStr, '', 'codMunic',    7,  7, 1, pEndereco.CodMunic);
  Gerador.wCampo(tcStr, '', 'uf',          2,  2, 1, eSufToStr(pEndereco.UF));

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

  Gerador.wCampo(tcStr, '', 'opcFGTS', 1, 1, 1, eSOpcFGTSToStr(pFGTS.OpcFGTS));

  if (eSOpcFGTSToStr(pFGTS.OpcFGTS) = '1') then
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

  Gerador.wCampo(tcDe2, '', 'qtdHrsSem', 0, 4, 0, pHorContratual.QtdHrsSem);
  Gerador.wCampo(tcStr, '', 'tpJornada', 1, 1, 1, eSTpJornadaToStr(pHorContratual.TpJornada));

  if (eSTpJornadaToStr(pHorContratual.TpJornada) = '9') then
    Gerador.wCampo(tcStr, '', 'dscTpJorn', 0, 100, 0, pHorContratual.DscTpJorn);

  Gerador.wCampo(tcStr, '', 'tmpParc', 1, 1, 1, tpTmpParcToStr(pHorContratual.tmpParc));

  GerarHorario(pHorContratual.horario);

  Gerador.wGrupo('/horContratual');
end;

procedure TeSocialEvento.GerarRemuneracao(pRemuneracao: TRemuneracao);
begin
  Gerador.wGrupo('remuneracao');

  Gerador.wCampo(tcDe2, '', 'vrSalFx',    1, 14, 1, pRemuneracao.VrSalFx);
  Gerador.wCampo(tcStr, '', 'undSalFixo', 1,  1, 1, eSUndSalFixoToStr(pRemuneracao.UndSalFixo));

  if (eSUndSalFixoToStr(pRemuneracao.UndSalFixo) = '7') then
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
  if pRNE.NrRne  <> EmptyStr then
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
  if pSucessaoVinc.cnpjEmpregAnt <> EmptyStr then
  begin
    Gerador.wGrupo('sucessaoVinc');
    Gerador.wCampo(tcStr, '', 'cnpjEmpregAnt', 14, 014, 1, pSucessaoVinc.cnpjEmpregAnt);
    Gerador.wCampo(tcStr, '', 'matricAnt',      0, 030, 0, pSucessaoVinc.MatricAnt);
    Gerador.wCampo(tcDat, '', 'dtTransf',      10, 010, 1, pSucessaoVinc.dtTransf);
    Gerador.wCampo(tcStr, '', 'observacao',     0, 255, 0, pSucessaoVinc.Observacao);
    Gerador.wGrupo('/sucessaoVinc');
  end;
end;

procedure TeSocialEvento.GerarTrabalhador(pTrabalhador: TTrabalhador; const GroupName: string;const tipo: Integer);
begin
  Gerador.wGrupo(GroupName);

  if (GroupName = 'trabalhador') then
    Gerador.wCampo(tcStr, '', 'cpfTrab', 11, 11, 1, pTrabalhador.CpfTrab);

  Gerador.wCampo(tcStr, '', 'nisTrab', 1, 11, 1, pTrabalhador.NisTrab);
  Gerador.wCampo(tcStr, '', 'nmTrab',  1, 70, 1, pTrabalhador.NmTrab);
  Gerador.wCampo(tcStr, '', 'sexo',    1,  1, 1, pTrabalhador.Sexo);
  Gerador.wCampo(tcStr, '', 'racaCor', 1,  1, 1, pTrabalhador.RacaCor);

  if ((pTrabalhador.EstCiv >= 1) and (pTrabalhador.EstCiv <= 5)) then
    Gerador.wCampo(tcStr, '', 'estCiv', 1, 1, 0, pTrabalhador.EstCiv);

  Gerador.wCampo(tcStr, '', 'grauInstr', 2, 2, 1, pTrabalhador.GrauInstr);

  if (tipo = 2) then
    Gerador.wCampo(tcStr, '', 'indPriEmpr', 1, 1, 0, eSSimNaoToStr(pTrabalhador.IndPriEmpr));

  Gerador.wCampo(tcStr, '', 'nmSoc', 1, 70, 0, pTrabalhador.nmSoc);

  if (GroupName = 'trabalhador') or
     ((GroupName = 'dadosTrabalhador') and (TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF = ve02_04_02)) then
    GerarNascimento(pTrabalhador.Nascimento);

  GerarDocumentos(pTrabalhador.Documentos);
  GerarEndereco(pTrabalhador.Endereco,pTrabalhador.ExtrangeiroSN);
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

    Gerador.wCampo(tcDat, '', 'dtChegada',        10, 10, 1, pTrabEstrangeiro.DtChegada);
    Gerador.wCampo(tcStr, '', 'classTrabEstrang',  1,  2, 1, eSClassTrabEstrangToStr(pTrabEstrangeiro.classTrabEstrang));
    Gerador.wCampo(tcStr, '', 'casadoBr',          1,  1, 1, pTrabEstrangeiro.CasadoBr);
    Gerador.wCampo(tcStr, '', 'filhosBr',          1,  1, 1, pTrabEstrangeiro.FilhosBr);

    Gerador.wGrupo('/trabEstrangeiro');
  end;
end;

procedure TeSocialEvento.GerarTrabTemporario(pTrabTemporario: TTrabTemporario);
begin
  if pTrabTemporario.hipLeg > 0 then
  begin
    Gerador.wGrupo('trabTemporario');

    Gerador.wCampo(tcInt, '', 'hipLeg',      1,   1, 1, pTrabTemporario.hipLeg);
    Gerador.wCampo(tcStr, '', 'justContr',   1, 999, 1, pTrabTemporario.justContr);
    Gerador.wCampo(tcInt, '', 'tpInclContr', 1,   1, 1, eSTpInclContrToStr(pTrabTemporario.tpinclContr));

    GerarIdeTomadorServ(pTrabTemporario.IdeTomadorServ);
    GerarIdeTrabSubstituido(pTrabTemporario.IdeTrabSubstituido);

    Gerador.wGrupo('/trabTemporario');
  end;
end;

procedure TeSocialEvento.GerarVinculo(pVinculo: TVinculo; pTipo: integer);
begin
  Gerador.wGrupo('vinculo');

  if (pTipo <> 3) then
  begin
    Gerador.wCampo(tcStr, '', 'matricula', 1, 30, 1, pVinculo.Matricula);
    Gerador.wCampo(tcStr, '', 'tpRegTrab', 1,  1, 1, eSTpRegTrabToStr(pVinculo.TpRegTrab));
  end;

  Gerador.wCampo(tcStr, '', 'tpRegPrev', 1, 1, 1, eSTpRegPrevToStr(pVinculo.TpRegPrev));

  if (pTipo <> 3) then
  begin
    if (((pVinculo.nrRecInfPrelim <> '') and (pVinculo.nrRecInfPrelim <> null)) and (pTipo = 2)) then
      Gerador.wCampo(tcStr, '', 'nrRecInfPrelim', 0, 40, 0, pVinculo.nrRecInfPrelim);

    Gerador.wCampo(tcStr, '', 'cadIni', 1, 1, 1, eSSimNaoToStr(pVinculo.cadIni));

    GerarInfoRegimeTrab(pVinculo.InfoRegimeTrab);
    GerarInfoContrato(pVinculo.InfoContrato, pTipo, pVinculo.InfoRegimeTrab);
    GerarSucessaoVinc(pVinculo.SucessaoVinc);
    GerarTransfDom(pVinculo.transfDom);

    if (pTipo = 1) or (pTipo = 2) then
    begin
      GerarAfastamento(pVinculo.Afastamento);
      GerarDesligamento(pVinculo.Desligamento);
    end;
  end;

  Gerador.wGrupo('/vinculo');
end;

procedure TeSocialEvento.GerarAfastamento(pAfastamento: TAfastamento);
begin
  if pAfastamento.DtIniAfast > 0 then
  begin
    Gerador.wGrupo('afastamento');

    Gerador.wCampo(tcDat, '', 'dtIniAfast',  10, 10, 1, pAfastamento.DtIniAfast);
    Gerador.wCampo(tcStr, '', 'codMotAfast',  1,  1, 1, eStpMotivosAfastamentoToStr(pAfastamento.codMotAfast));

    Gerador.wGrupo('/afastamento');
  end;
end;

procedure TeSocialEvento.GerarAliqGilRat(pEmp: TIdeEmpregador; pAliqRat: TAliqGilRat;
  const GroupName: string);
begin
  Gerador.wGrupo(GroupName);

  Gerador.wCampo(tcStr, '', 'aliqRat',      1, 1, 1, eSAliqRatToStr(pAliqRat.AliqRat));

  if (pEmp.TpInsc = tiCNPJ) then
  begin
    Gerador.wCampo(tcDe4, '', 'fap',          1, 5, 0, pAliqRat.Fap);
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

  if (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador in [teOrgaoPublico, tePessoaFisica]) then
    Gerador.wCampo(tcStr, '', 'nrInsc', 14, 14, 1, pEmp.NrInsc)
  else
    Gerador.wCampo(tcStr, '', 'nrInsc', 8, 8, 1, Copy(pEmp.NrInsc, 1, 8));

  Gerador.wGrupo('/ideEmpregador');
end;

procedure TeSocialEvento.GerarIdeEvento(pEvt: TIdeEvento; const GeraGrupo: boolean);
begin
  if GeraGrupo then
    Gerador.wGrupo('ideEvento');

  Gerador.wCampo(tcStr, '', 'tpAmb',   1,  1, 1, eStpAmbToStr(pEvt.TpAmb));
  Gerador.wCampo(tcStr, '', 'procEmi', 1,  1, 1, eSProcEmiToStr(pEvt.ProcEmi));
  Gerador.wCampo(tcStr, '', 'verProc', 1, 20, 1, pEvt.VerProc);

  if GeraGrupo then
  	Gerador.wGrupo('/ideEvento');
end;

procedure TeSocialEvento.GerarIdeEvento2(pEvt: TIdeEvento2; const GeraGrupo: boolean = True; GeraRetif: Boolean = True);
begin
  if GeraGrupo then
    Gerador.wGrupo('ideEvento');

  if (GeraRetif) then
    Gerador.wCampo(tcStr, '', 'indRetif', 1, 1, 1, eSIndRetificacaoToStr(pEvt.indRetif));

  if (eSIndRetificacaoToStr(pEvt.indRetif) = '2') then
    Gerador.wCampo(tcStr, '', 'nrRecibo', 1, 40, 0, pEvt.nrRecibo);

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

  Gerador.wCampo(tcStr, '', 'indApuracao', 1, 1, 1, eSIndApuracaoToStr(pEvt.IndApuracao));
  Gerador.wCampo(tcStr, '', 'perApur',     7, 7, 1, pEvt.perApur);

  GerarIdeEvento(pEvt, false);

  Gerador.wGrupo('/ideEvento');
end;

procedure TeSocialEvento.GerarIdeEvento4(pEvt: TIdeEvento4);
begin
  Gerador.wGrupo('ideEvento');

  Gerador.wCampo(tcStr, '', 'indApuracao', 1,  1, 1, eSIndApuracaoToStr(pEvt.IndApuracao));
  Gerador.wCampo(tcStr, '', 'perApur',     7,  7, 1, pEvt.perApur);
  Gerador.wCampo(tcStr, '', 'tpAmb',       1,  1, 1, eStpAmbToStr(pEvt.TpAmb));
  Gerador.wCampo(tcStr, '', 'procEmi',     1,  1, 1, eSProcEmiToStr(pEvt.ProcEmi));
  Gerador.wCampo(tcStr, '', 'verProc',     1, 20, 1, pEvt.VerProc);

  Gerador.wGrupo('/ideEvento');
end;

procedure TeSocialEvento.GerarIdeEvento5(pEvt: TIdeEvento5; nrRecArqBase,
  IndApuracao: Boolean);
begin
  Gerador.wGrupo('ideEvento');

  if nrRecArqBase then
    Gerador.wCampo(tcStr, '', 'nrRecArqBase', 1, 40, 0, pEvt.nrRecArqBase);

  if IndApuracao then
    Gerador.wCampo(tcStr, '', 'indApuracao', 1,  1, 1, eSIndApuracaoToStr(pEvt.IndApuracao));

  Gerador.wCampo(tcStr, '', 'perApur',     7,  7, 1, pEvt.perApur);

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
  if (pIdeEstabVinc.NrInsc <> '') then
  begin
    Gerador.wGrupo('ideEstabVinc');

    Gerador.wCampo(tcStr, '', 'tpInsc', 1,  1, 1, eSTpInscricaoToStr(pIdeEstabVinc.TpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc', 1, 15, 1, pIdeEstabVinc.NrInsc);

    Gerador.wGrupo('/ideEstabVinc');
  end;
end;

procedure TeSocialEvento.GerarIdeTomadorServ(pIdeTomadorServ: TIdeTomadorServ);
begin
  Gerador.wGrupo('ideTomadorServ');

  Gerador.wCampo(tcStr, '', 'tpInsc', 1,  1, 1, eSTpInscricaoToStr(pIdeTomadorServ.TpInsc));
  Gerador.wCampo(tcStr, '', 'nrInsc', 1, 15, 1, pIdeTomadorServ.NrInsc);

  GerarIdeEstabVinc(pIdeTomadorServ.ideEstabVinc);

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

procedure TeSocialEvento.GerarIdeVinculo(pIdeVinculo: TIdeVinculo);
begin
  Gerador.wGrupo('ideVinculo');

  Gerador.wCampo(tcStr, '', 'cpfTrab',   11, 11, 1, pIdeVinculo.cpfTrab);
  Gerador.wCampo(tcStr, '', 'nisTrab',    1, 11, 1, pIdeVinculo.nisTrab);
  Gerador.wCampo(tcStr, '', 'matricula',  1, 30, 0, pIdeVinculo.matricula);
  Gerador.wCampo(tcInt, '', 'codCateg',   1,  3, 0, pIdeVinculo.codCateg);

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

    Gerador.wCampo(tcStr, '', 'tpInsc', 1,  1, 1, eSTpInscricaoToStr(pAprend.TpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc', 1, 15, 1, pAprend.NrInsc);

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
    Gerador.wCampo(tcStr, '', 'tpRegJor',      1,  1, 1, eSTpRegJorToStr(pInfoCeletista.TpRegJor));
    Gerador.wCampo(tcStr, '', 'natAtividade',  1,  1, 1, eSNatAtividadeToStr(pInfoCeletista.NatAtividade));

    if ((pInfoCeletista.dtBase >= 1) and (pInfoCeletista.dtBase <= 12)) then
      Gerador.wCampo(tcStr, '', 'dtBase', 0, 2, 0, pInfoCeletista.dtBase);

    Gerador.wCampo(tcStr, '', 'cnpjSindCategProf', 14, 14, 1, pInfoCeletista.cnpjSindCategProf);

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

    if (pTipo <> 3) then
      Gerador.wCampo(tcStr, '', 'infoCota', 1, 1, 1, eSSimNaoToStr(pInfoDeficiencia.infoCota));

    Gerador.wCampo(tcStr, '', 'observacao', 1, 255, 0, pInfoDeficiencia.Observacao);

    Gerador.wGrupo('/infoDeficiencia');
  end;
end;

procedure TeSocialEvento.GerarInfoDecJud(pInfoDecJud: TInfoDecJud);
begin
  if (pInfoDecJud.nrProcJud <> '') then
  begin
    Gerador.wGrupo('infoDecJud');

    Gerador.wCampo(tcStr, '', 'nrProcJud', 1, 20, 1, pInfoDecJud.nrProcJud);

   Gerador.wGrupo('/infoDecJud');
  end;
end;

procedure TeSocialEvento.GerarInfoEstatutario(
  pInfoEstatutario: TInfoEstatutario);
begin
  if pInfoEstatutario.DtNomeacao > 0 then
  begin
    Gerador.wGrupo('infoEstatutario');

    Gerador.wCampo(tcStr, '', 'indProvim',   1,  1, 1, eSIndProvimToStr(pInfoEstatutario.IndProvim));
    Gerador.wCampo(tcStr, '', 'tpProv',      1,  2, 1, eSTpProvToStr(pInfoEstatutario.TpProv));
    Gerador.wCampo(tcDat, '', 'dtNomeacao', 10, 10, 1, pInfoEstatutario.DtNomeacao);
    Gerador.wCampo(tcDat, '', 'dtPosse',    10, 10, 0, pInfoEstatutario.DtPosse);

    if ((eSIndProvimToStr(pInfoEstatutario.IndProvim) = '1') or
        (eSIndProvimToStr(pInfoEstatutario.IndProvim) = '2') or
        (DateToStr(pInfoEstatutario.DtExercicio) <> '')) then
      Gerador.wCampo(tcDat, '', 'dtExercicio ', 10, 10, 0, pInfoEstatutario.DtExercicio);

    Gerador.wCampo(tcInt, '', 'tpPlanRP', 0, 1, 0,  eSTpPlanRPToStr(pInfoEstatutario.tpPlanRP));

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
  i: Integer;
begin
  for i := 0 to objItensRemun.Count - 1 do
  begin
    Gerador.wGrupo(GroupName);

    Gerador.wCampo(tcStr, '', 'codRubr',    1, 30, 1, objItensRemun.Items[i].codRubr);
    Gerador.wCampo(tcStr, '', 'ideTabRubr', 1,  8, 1, objItensRemun.Items[i].ideTabRubr);
    Gerador.wCampo(tcDe2, '', 'qtdRubr',    1,  6, 0, objItensRemun.Items[i].qtdRubr);
    Gerador.wCampo(tcDe2, '', 'fatorRubr',  1,  5, 0, objItensRemun.Items[i].fatorRubr);
    Gerador.wCampo(tcDe2, '', 'vrUnit',     1, 14, 0, objItensRemun.Items[i].vrUnit);
    Gerador.wCampo(tcDe2, '', 'vrRubr',     1, 14, 1, objItensRemun.Items[i].vrRubr);

    Gerador.wGrupo('/' + GroupName);
  end;

  if objItensRemun.Count > 200 then
    Gerador.wAlerta('', GroupName, 'Lista de ' + GroupName, ERR_MSG_MAIOR_MAXIMO + '200');
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

    Gerador.wCampo(tcStr, '', 'tpLograd',    1,  4, 1, pLocalTrabDom.TpLograd);
    Gerador.wCampo(tcStr, '', 'dscLograd',   1, 80, 1, pLocalTrabDom.DscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd',    1, 10, 1, pLocalTrabDom.NrLograd);
    Gerador.wCampo(tcStr, '', 'complemento', 0, 30, 0,  pLocalTrabDom.Complemento);
    Gerador.wCampo(tcStr, '', 'bairro',      0, 60, 0, pLocalTrabDom.Bairro);
    Gerador.wCampo(tcStr, '', 'cep',         1,  8, 1, pLocalTrabDom.Cep);
    Gerador.wCampo(tcStr, '', 'codMunic',    7,  7, 1,  pLocalTrabDom.CodMunic);
    Gerador.wCampo(tcStr, '', 'uf',          2,  2, 1, eSufToStr(pLocalTrabDom.Uf));

    Gerador.wGrupo('/localTrabDom');
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

procedure TeSocialEvento.GerarNascimento(pNasc: TNascimento; pGroupName: string = 'nascimento');
begin
  Gerador.wGrupo(pGroupName);

  Gerador.wCampo(tcDat, '', 'dtNascto', 10, 10, 1, pNasc.DtNascto);

  if pNasc.CodMunic <> 0 then
    Gerador.wCampo(tcStr, '', 'codMunic', 1, 7, 0, pNasc.CodMunic);

  Gerador.wCampo(tcStr, '', 'uf',         2,  2, 0, pNasc.UF);
  Gerador.wCampo(tcStr, '', 'paisNascto', 1,  3, 1, pNasc.PaisNascto);
  Gerador.wCampo(tcStr, '', 'paisNac',    1,  3, 1, pNasc.PaisNac);
  Gerador.wCampo(tcStr, '', 'nmMae',      1, 70, 0, pNasc.NmMae);
  Gerador.wCampo(tcStr, '', 'nmPai',      1, 70, 0, pNasc.NmPai);

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

procedure TeSocialEvento.GerarProcessoGenerico(pChave: string; pProcesso: TProcesso);
begin
  Gerador.wGrupo(pChave);

  Gerador.wCampo(tcStr, '', 'nrProc', 1, 21, 1, pProcesso.nrProc);

  if trim(pProcesso.codSusp) <> '' then
    Gerador.wCampo(tcStr, '', 'codSusp', 1, 14, 1, pProcesso.codSusp);

  Gerador.wGrupo('/' + pChave);
end;

procedure TeSocialEvento.GerarProcessoAdmJudFap(pProcAdmJudFap: TProcAdmJudFap);
begin
  Gerador.wGrupo('procAdmJudFap');

  Gerador.wCampo(tcStr, '', 'tpProc', 1,  1, 1, eSTpProcessoToStr(pProcAdmJudFap.tpProc));
  Gerador.wCampo(tcStr, '', 'nrProc', 1, 20, 1, pProcAdmJudFap.nrProc);

  if trim(pProcAdmJudFap.codSusp) <> '' then
    Gerador.wCampo(tcStr, '', 'codSusp', 1, 14, 1, pProcAdmJudFap.codSusp);

  Gerador.wGrupo('/procAdmJudFap');
end;

procedure TeSocialEvento.GerarProcessoAdmJudRat(pProcAdmJudRat: TProcAdmJudRat);
begin
  Gerador.wGrupo('procAdmJudRat');

  Gerador.wCampo(tcStr, '', 'tpProc', 1,  1, 1, eSTpProcessoToStr(pProcAdmJudRat.tpProc));
  Gerador.wCampo(tcStr, '', 'nrProc', 1, 20, 1, pProcAdmJudRat.nrProc);

  if trim(pProcAdmJudRat.codSusp) <> '' then
    Gerador.wCampo(tcStr, '', 'codSusp', 1, 14, 1, pProcAdmJudRat.codSusp);

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

  Gerador.wCampo(tcStr, '', 'nisTrab', 1, 11, 0, pideTrabalhador.nisTrab);

  if GeraGrupo then
    Gerador.wGrupo('/ideTrabalhador');
end;

procedure TeSocialEvento.GerarIdeTrabalhador3(
  pideTrabalhador: TideTrabalhador3; const tpTrib: boolean);
begin
  Gerador.wGrupo('ideTrabalhador');

  Gerador.wCampo(tcStr, '', 'cpfTrab', 11, 11, 1, pideTrabalhador.CpfTrab);

  GerarProcJudTrab(pideTrabalhador.procJudTrab, False);

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

procedure TeSocialEvento.GerarEmitente(pEmitente: TEmitente);
begin
  Gerador.wGrupo('emitente');

  Gerador.wCampo(tcStr, '', 'nmEmit', 1, 70, 1, pEmitente.nmEmit);
  Gerador.wCampo(tcStr, '', 'ideOC',  1,  1, 1, eSIdeOCToStr(pEmitente.ideOC));
  Gerador.wCampo(tcStr, '', 'nrOc',   1, 14, 1, pEmitente.nrOc);
  Gerador.wCampo(tcStr, '', 'ufOC',   2,  2, 0, eSufToStr(pEmitente.ufOC));

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
      Gerador.wCampo(tcStr, '', 'serie',        1,  5, 0, pNfs.Items[i].serie);
      Gerador.wCampo(tcStr, '', 'nrDocto',      1, 20, 1, pNfs.Items[i].nrDocto);
      Gerador.wCampo(tcDat, '', 'dtEmisNF',    10, 10, 1, pNfs.Items[i].dtEmisNF);
      Gerador.wCampo(tcDe2, '', 'vlrBruto',     1, 14, 1, pNfs.Items[i].vlrBruto);
      Gerador.wCampo(tcDe2, '', 'vrCPDescPR',   1, 14, 1, pNfs.Items[i].vrCPDescPR);
      Gerador.wCampo(tcDe2, '', 'vrRatDescPR',  1, 14, 1, pNfs.Items[i].vrRatDescPR);
      Gerador.wCampo(tcDe2, '', 'vrSenarDesc',  1, 14, 1, pNfs.Items[i].vrSenarDesc);
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
      Gerador.wCampo(tcStr, '', 'codCateg',   1,  3, 1, objRemunOutrEmpr.Items[i].codCateg);
      Gerador.wCampo(tcDe2, '', 'vlrRemunOE', 1, 14, 1, objRemunOutrEmpr.Items[i].vlrRemunOE);

      Gerador.wGrupo('/remunOutrEmpr');
    end;
  end;

  if objRemunOutrEmpr.Count > 10 then
    Gerador.wAlerta('', 'remunOutrEmpr', 'Lista de Informações de Remuneração', ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TeSocialEvento.GerarInfoMV(pInfoMV: TInfoMV);
begin
  if Ord(pInfoMV.indMV) > 0 then
  begin
    Gerador.wGrupo('infoMV');

    Gerador.wCampo(tcStr, '', 'indMV', 1, 1, 1, eSIndMVToStr(pInfoMV.indMV));

    GerarRemunOutrEmpr(pInfoMV.remunOutrEmpr);

    Gerador.wGrupo('/infoMV');
  end;
end;

procedure TeSocialEvento.GerarInfoSimples(obj: TinfoSimples);
begin
  Gerador.wGrupo('infoSimples');

  Gerador.wCampo(tcStr, '', 'indSimples', 1, 1, 1, obj.indSimples);

  Gerador.wGrupo('/infoSimples');
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

    if pIdeEstabLot[i].infoSaudeColetInst then
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
  if obj.dtFimQuar > 0 then
  begin
    Gerador.wGrupo('quarentena');

    Gerador.wCampo(tcDat, '', 'dtFimQuar', 10, 10, 1, obj.dtFimQuar);

    Gerador.wGrupo('/quarentena');
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
    Gerador.wCampo(tcStr, '', 'complem',   0, 30, 0,  pEndereco.Complemento);
    Gerador.wCampo(tcStr, '', 'bairro',    0, 60, 0, pEndereco.Bairro);
    Gerador.wCampo(tcStr, '', 'cep',       1,  8, 1, pEndereco.Cep);
    Gerador.wCampo(tcStr, '', 'codMunic',  7,  7, 1,  pEndereco.CodMunic);
    Gerador.wCampo(tcStr, '', 'uf',        2,  2, 1, eSufToStr(pEndereco.Uf));

    Gerador.wGrupo('/localTrabInterm');
  end;
end;

end.

