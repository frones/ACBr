{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibNFeClass;

interface

uses
  Classes, SysUtils, Forms,
  ACBrLibComum, ACBrLibNFeDataModule;

type

  { TACBrLibNFe }

  TACBrLibNFe = class(TACBrLib)
  private
    FNFeDM: TLibNFeDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property NFeDM: TLibNFeDM read FNFeDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function NFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region NFe}
function NFE_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Assinar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Validar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Servicos}
function RespostaCancelamento: String;
function RespostaDistribuicaoDFe: String;
function RespostaItensDistribuicaoDFeResNFe(ItemID: integer = 0): String;
function RespostaItensDistribuicaoDFeResEve(ItemID: integer = 0): String;
function RespostaItensDistribuicaoDFeProEve(ItemID: integer = 0): String;
function RespostaItensDistribuicaoDFeInfeve(ItemID: integer = 0): String;
function RespostaEnvio: String;
function RespostaRetorno: String;
function RespostaEvento: String;
function RespostaItensEvento(ItemID: integer = 0): String;

function NFE_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Consultar(const eChaveOuNFe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Inutilizar(const ACNPJ, AJustificativa: PChar;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Enviar(ALote: Integer; Imprimir: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_EnviarEvento(idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_DistribuicaoDFePorUltNSU(const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_DistribuicaoDFePorNSU(const AcUFAutor: integer; eCNPJCPF, eNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_DistribuicaoDFePorChave(const AcUFAutor: integer; eCNPJCPF, echNFe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_EnviarEmail(const ePara, eChaveNFe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_EnviarEmailEvento(const ePara, eChaveEvento, eChaveNFe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Imprimir(const cImpressora: PChar; nNumCopias: Integer; const cProtocolo,
  bMostrarPreview, cMarcaDagua, bViaConsumidor, bSimplificado: PChar): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ImprimirEvento(const eChaveNFe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ImprimirEventoPDF(const eChaveNFe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ImprimirInutilizacao(const eChave: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ImprimirInutilizacaoPDF(const eChave: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibNFeConsts, ACBrLibConfig, ACBrLibResposta,
  ACBrLibNFeConfig, ACBrLibNFeRespostas, ACBrNFe, ACBrMail,
  pcnConversao, pcnAuxiliar, blcksock, ACBrUtil;

{ TACBrLibNFe }

constructor TACBrLibNFe.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibNFeNome;
  fpVersao := CLibNFeVersao;

  FNFeDM := TLibNFeDM.Create(nil);
end;

destructor TACBrLibNFe.Destroy;
begin
  FNFeDM.Free;

  inherited Destroy;
end;

procedure TACBrLibNFe.Inicializar;
begin
  GravarLog('TACBrLibNFe.Inicializar', logNormal);

  FNFeDM.CriarACBrMail;
  FNFeDM.CriarACBrPosPrinter;

  GravarLog('TACBrLibNFe.Inicializar - Feito', logParanoico);

  inherited Inicializar;
end;

procedure TACBrLibNFe.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibNFeConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibNFe.Executar;
begin
  inherited Executar;
  FNFeDM.AplicarConfiguracoes;
end;

{%region NFe}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function NFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function NFE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function NFE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function NFE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function NFE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function NFE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function NFE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function NFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function NFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;

{%endregion}

function NFE_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuXml := string(eArquivoOuXML);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFE_CarregarXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFE_CarregarXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        if EhArquivo then
          NFeDM.ACBrNFe1.NotasFiscais.LoadFromFile(ArquivoOuXml)
        else
          NFeDM.ACBrNFe1.NotasFiscais.LoadFromString(ArquivoOuXml);

        Result := SetRetornoNFeCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ArquivoOuINI: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuINI := string(eArquivoOuINI);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFE_CarregarINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFE_CarregarINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        NFeDM.ACBrNFe1.NotasFiscais.LoadFromIni(ArquivoOuINI);
        Result := SetRetornoNFeCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFE_LimparLista', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        NFeDM.ACBrNFe1.NotasFiscais.Clear;
        Result := SetRetornoNFeCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_Assinar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFe_Assinar', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        try
          NFeDM.ACBrNFe1.NotasFiscais.Assinar;
        except
          on E: EACBrNFeException do
            Result := SetRetorno(ErrAssinarNFe, E.Message);
        end;
        Result := SetRetornoNFeCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_Validar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFE_Validar', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        try
          NFeDM.ACBrNFe1.NotasFiscais.Validar;
        except
          on E: EACBrNFeException do
            Result := SetRetorno(ErrValidacaoNFe, E.Message);
        end;
        Result := SetRetornoNFeCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Erros: string;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFE_ValidarRegrasdeNegocios', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        Erros := '';
        NFeDM.ACBrNFe1.NotasFiscais.ValidarRegrasdeNegocios(Erros);
        MoverStringParaPChar(Erros, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Erros: string;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFE_VerificarAssinatura', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        Erros := '';
        NFeDM.ACBrNFe1.NotasFiscais.VerificarAssinatura(Erros);
        MoverStringParaPChar(Erros, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

{%endregion}

{%region Servicos}

function RespostaCancelamento: String;
var
  Resp: TCancelamentoResposta;
  Resposta: String;
begin
  Resp := TCancelamentoResposta.Create(resINI);
  try
    with TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfevento do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cOrgao;
      Resp.ChNFe := chNFe;
      Resp.DhRecbto := dhRegEvento;
      Resp.NProt := nProt;
      Resp.TpEvento := TpEventoToStr(tpEvento);
      Resp.xEvento := xEvento;
      Resp.nSeqEvento := nSeqEvento;
      Resp.CNPJDest := CNPJDest;
      Resp.emailDest := emailDest;
      Resp.XML := XML;

      Resposta := XMotivo + sLineBreak;
      Resposta := Resposta + Resp.Gerar;

      Result := Resposta;
    end;
  finally
    Resp.Free;
  end;
end;

function RespostaDistribuicaoDFe: String;
var
  Resp: TDistribuicaoDFeResposta;
  sTemMais: String;
begin
  Resp := TDistribuicaoDFeResposta.Create(resINI);
  try
    with TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.DistribuicaoDFe do
    begin
      Resp.arquivo := NomeArq;

      with retDistDFeInt do
      begin
        Resp.Versao := versao;
        Resp.VerAplic := VerAplic;
        Resp.tpAmb := TpAmbToStr(tpAmb);
        Resp.CStat := cStat;
        Resp.XMotivo := XMotivo;
        Resp.dhResp := dhResp;
        Resp.ultNSU := ultNSU;
        Resp.maxNSU := maxNSU;

        if cStat = 137 then
          sTemMais := '1'  // Sim
        else
          sTemMais := '0'; // Não

        Resp.indCont := sTemMais;

        Result := Resp.Gerar;
      end;
    end;
  finally
    Resp.Free;
  end;
end;

function RespostaItensDistribuicaoDFeResNFe(ItemID: integer = 0): String;
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  Resp := TDistribuicaoDFeItemResposta.Create(
    'ResNFe' + Trim(IntToStrZero(ItemID +1, 3)), resINI);
  try
    with TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.DistribuicaoDFe do
    begin
      Resp.NSU := retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.XML := retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := listaArqs[ItemID];
      Resp.schema := SchemaDFeToStr(retDistDFeInt.docZip[ItemID].schema);

      with retDistDFeInt.docZip.Items[ItemID].resDFe do
      begin
        Resp.chNFe := chNFe;
        Resp.CNPJCPF := CNPJCPF;
        Resp.xNome := xNome;
        Resp.IE := IE;
        Resp.dhEmi := dhEmi;
        Resp.vNF := vNF;
        Resp.digVal := digVal;
        Resp.dhRecbto := dhRecbto;
        Resp.cSitNFe := SituacaoDFeToStr(cSitDFe);
        Resp.nProt := nProt;

        Result := Resp.Gerar;
      end;
    end;
  finally
    Resp.Free;
  end;
end;

function RespostaItensDistribuicaoDFeResEve(ItemID: integer = 0): String;
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  Resp := TDistribuicaoDFeItemResposta.Create(
    'ResEve' + Trim(IntToStrZero(ItemID +1, 3)), resINI);
  try
    // Atualmente o DistribuicaoDFe do CT-e não retorna Resumo de Eventos.
    with TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].resEvento do
    begin
      Resp.NSU := TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.chNFe := chDFe;
      Resp.CNPJCPF := CNPJCPF;
      Resp.dhEvento := dhEvento;
      Resp.tpEvento := TpEventoToStr(tpEvento);
      Resp.xEvento := xEvento;
      Resp.nSeqEvento := nSeqEvento;
      Resp.cOrgao := cOrgao;
      Resp.dhRecbto := dhRecbto;
      Resp.nProt := nProt;
      Resp.XML := TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.DistribuicaoDFe.listaArqs[ItemID];
      Resp.schema := SchemaDFeToStr(TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[ItemID].schema);

      Result := Resp.Gerar;
    end;
    Result := '';
  finally
    Resp.Free;
  end;
end;

function RespostaItensDistribuicaoDFeProEve(ItemID: integer = 0): String;
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  Resp := TDistribuicaoDFeItemResposta.Create(
    'ProEve' + Trim(IntToStrZero(ItemID +1, 3)), resINI);
  try
    with TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.DistribuicaoDFe do
    begin
      Resp.NSU := retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.XML := retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := listaArqs[ItemID];
      Resp.schema := SchemaDFeToStr(retDistDFeInt.docZip[ItemID].schema);

      with retDistDFeInt.docZip.Items[ItemID].procEvento do
      begin
        Resp.chNFe := chNFe;
        Resp.cOrgao := cOrgao;
        Resp.CNPJ := CNPJ;
        Resp.Id := Id;
        Resp.dhEvento := dhEvento;
        Resp.nSeqEvento := nSeqEvento;
        Resp.tpAmb := TpAmbToStr(tpAmb);
        Resp.tpEvento := TpEventoToStr(tpEvento);
        Resp.verEvento := verEvento;

        with detEvento do
        begin
          Resp.descEvento := descEvento;
          Resp.xJust := xJust;
          Resp.EmiCnpj := emit.CNPJ;
          Resp.EmiIE := emit.IE;
          Resp.EmixNome := emit.xNome;
          Resp.cteNProt := CTe.nProt;
          Resp.cteChvCTe := CTe.chCTe;
          Resp.cteDhemi := CTe.dhEmi;
          Resp.cteModal := TpModalToStr(CTe.modal);
          Resp.cteDhRebcto := CTe.dhRecbto;
        end;

        Result := Resp.Gerar;
      end;
    end;
  finally
    Resp.Free;
  end;
end;

function RespostaItensDistribuicaoDFeInfeve(ItemID: integer = 0): String;
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  Resp := TDistribuicaoDFeItemResposta.Create(
    'Infeve' + Trim(IntToStrZero(ItemID +1, 3)), resINI);
  try
    with TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.DistribuicaoDFe do
    begin
      Resp.XML := retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := listaArqs[ItemID];
      Resp.schema := SchemaDFeToStr(retDistDFeInt.docZip[ItemID].schema);

      with retDistDFeInt.docZip.Items[ItemID].procEvento.RetInfevento do
      begin
        Resp.Id := Id;
        Resp.VerAplic := VerAplic;
        Resp.tpAmb := TpAmbToStr(tpAmb);
        Resp.cOrgao := cOrgao;
        Resp.chNFe := chNFe;
        Resp.CStat := cStat;
        Resp.CNPJDest := CNPJDest;
        Resp.cOrgaoAutor := cOrgaoAutor;
        Resp.tpEvento := TpEventoToStr(tpEvento);
        Resp.nSeqEvento := nSeqEvento;
        Resp.xEvento := xEvento;
        Resp.XMotivo := XMotivo;
        Resp.dhRegEvento := dhRegEvento;
        Resp.emailDest := emailDest;
        Resp.nProt := nProt;

        Result := Resp.Gerar;
      end;
    end;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvio: String;
var
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(resINI);
  try
    with TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.Enviar do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.verAplic := verAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := xMotivo;
      Resp.CUF := cUF;
      Resp.nRec := Recibo;
      Resp.DhRecbto := dhRecbto;
      Resp.Tmed := TMed;
      Resp.Msg := Msg;

      Result := sLineBreak + Msg + sLineBreak + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

function RespostaRetorno: String;
var
  Resp: TRetornoResposta;
begin
  Resp := TRetornoResposta.Create(resINI);
  try
    with TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.Retorno do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.verAplic := verAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := xMotivo;
      Resp.CUF := cUF;
      Resp.nRec := Recibo;
      Resp.Msg := Msg;

      Result := sLineBreak + Msg + sLineBreak + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

function RespostaEvento: String;
var
  Resp: TEventoResposta;
begin
  Resp := TEventoResposta.Create(resINI);
  try
    with TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.EnvEvento.EventoRetorno do
    begin
      Resp.VerAplic := VerAplic;
      Resp.tpAmb := TpAmbToStr(tpAmb);
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.idLote := IdLote;
      Resp.cOrgao := cOrgao;

      Result := Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

function RespostaItensEvento(ItemID: integer = 0): String;
var
  Resp: TEventoItemResposta;
begin
  Resp := TEventoItemResposta.Create(
    'EVENTO' + Trim(IntToStrZero(ItemID +1, 3)), resINI);
  try
    with TACBrLibNFe(pLib).NFeDM.ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[ItemID].RetInfevento do
    begin
      Resp.Id := Id;
      Resp.tpAmb := TpAmbToStr(tpAmb);
      Resp.verAplic := verAplic;
      Resp.cOrgao := cOrgao;
      Resp.cStat := cStat;
      Resp.xMotivo := xMotivo;
      Resp.chNFe := chNFe;
      Resp.tpEvento := TpEventoToStr(tpEvento);
      Resp.xEvento := xEvento;
      Resp.nSeqEvento := nSeqEvento;
      Resp.CNPJDest := CNPJDest;
      Resp.emailDest := emailDest;
      Resp.dhRegEvento := dhRegEvento;
      Resp.nProt := nProt;
      Resp.Arquivo := NomeArquivo;
      Resp.XML := XML;

      Result := Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

function NFE_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: TStatusServicoResposta;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFE_StatusServico', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      Resposta := TStatusServicoResposta.Create(pLib.Config.TipoResposta);
      try
        with NFeDM.ACBrNFe1 do
        begin
          if WebServices.StatusServico.Executar then
          begin
            Resposta.Msg := WebServices.StatusServico.Msg;
            Resposta.Versao := WebServices.StatusServico.versao;
            Resposta.TpAmb := TpAmbToStr(WebServices.StatusServico.TpAmb);
            Resposta.VerAplic := WebServices.StatusServico.VerAplic;
            Resposta.CStat := WebServices.StatusServico.CStat;
            Resposta.XMotivo := WebServices.StatusServico.XMotivo;
            Resposta.CUF := WebServices.StatusServico.CUF;
            Resposta.DhRecbto := WebServices.StatusServico.DhRecbto;
            Resposta.TMed := WebServices.StatusServico.TMed;
            Resposta.DhRetorno := WebServices.StatusServico.DhRetorno;
            Resposta.XObs := WebServices.StatusServico.XObs;

            MoverStringParaPChar(Resposta.Gerar, sResposta, esTamanho);
            Result := SetRetorno(ErrOK, StrPas(sResposta));
          end
          else
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'StatusServico');
        end;
      finally
        Resposta.Free;
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_Consultar(const eChaveOuNFe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ChaveOuNFe: string;
  Resposta: TConsultaNFeResposta;
begin
  try
    VerificarLibInicializada;

    ChaveOuNFe := string(eChaveOuNFe);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFE_Consultar(' + ChaveOuNFe + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFE_Consultar', logNormal);

    EhArquivo := StringEhArquivo(ChaveOuNFe);
    if EhArquivo then
      VerificarArquivoExiste(ChaveOuNFe);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;

      if EhArquivo then
        NFeDM.ACBrNFe1.NotasFiscais.LoadFromFile(ChaveOuNFe);

      if NFeDM.ACBrNFe1.NotasFiscais.Count = 0 then
      begin
        if ValidarChave(ChaveOuNFe) then
          NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave := ChaveOuNFe
        else
          raise EACBrLibException.Create(ErrChaveNFe, Format(SErrChaveInvalida, [ChaveOuNFe]));
      end
      else
        NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave := StringReplace(
          NFeDM.ACBrNFe1.NotasFiscais.Items[NFeDM.ACBrNFe1.NotasFiscais.Count - 1].NFe.infNFe.ID,
          'NFe','',[rfIgnoreCase]);

      Resposta := TConsultaNFeResposta.Create(pLib.Config.TipoResposta);
      try
        with NFeDM.ACBrNFe1 do
        begin
          if WebServices.Consulta.Executar then
          begin
            Resposta.Msg := WebServices.Consulta.Msg;
            Resposta.Versao := WebServices.Consulta.versao;
            Resposta.TpAmb := TpAmbToStr(WebServices.Consulta.TpAmb);
            Resposta.VerAplic := WebServices.Consulta.VerAplic;
            Resposta.CStat := WebServices.Consulta.CStat;
            Resposta.XMotivo := WebServices.Consulta.XMotivo;
            Resposta.CUF := WebServices.Consulta.CUF;
            Resposta.DhRecbto := WebServices.Consulta.DhRecbto;
            Resposta.ChNFe := WebServices.Consulta.NFeChave;
            Resposta.NProt := WebServices.Consulta.Protocolo;
            Resposta.DigVal := WebServices.Consulta.protNFe.digVal;

            MoverStringParaPChar(Resposta.Gerar, sResposta, esTamanho);
            Result := SetRetorno(ErrOK, StrPas(sResposta));
          end
          else
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'StatusServico');
        end;
      finally
        Resposta.Free;
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_Inutilizar(const ACNPJ, AJustificativa: PChar;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: TInutilizarNFeResposta;
  CNPJ, Justificativa: string;
begin
  try
    VerificarLibInicializada;

    Justificativa := string(AJustificativa);
    CNPJ := string(ACNPJ);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFE_InutilizarNFe(' + CNPJ + ',' + Justificativa + ',' + IntToStr(Ano) + ',' +
        IntToStr(modelo) + ',' + IntToStr(Serie) +  ',' + IntToStr(NumeroInicial) +  ',' +
        IntToStr(NumeroFinal) + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFE_InutilizarNFe', logNormal);

    CNPJ := OnlyNumber(CNPJ);

    if not ValidarCNPJ(CNPJ) then
       raise EACBrNFeException.Create('CNPJ: ' + CNPJ + ', inválido.');

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      Resposta := TInutilizarNFeResposta.Create(pLib.Config.TipoResposta);
      try
        with NFeDM.ACBrNFe1 do
        begin
          with WebServices do
          begin
            Inutilizacao.CNPJ := CNPJ;
            Inutilizacao.Justificativa := Justificativa;
            Inutilizacao.Modelo := Modelo;
            Inutilizacao.Serie := Serie;
            Inutilizacao.Ano := Ano;
            Inutilizacao.NumeroInicial := NumeroInicial;
            Inutilizacao.NumeroFinal := NumeroFinal;

            if Inutilizacao.Executar then
            begin
              Resposta.Msg := Inutilizacao.Msg;
              Resposta.Versao := Inutilizacao.versao;
              Resposta.TpAmb := TpAmbToStr(Inutilizacao.TpAmb);
              Resposta.VerAplic := Inutilizacao.VerAplic;
              Resposta.CStat := Inutilizacao.CStat;
              Resposta.XMotivo := Inutilizacao.XMotivo;
              Resposta.CUF := Inutilizacao.cUF;
              Resposta.DhRecbto := Inutilizacao.DhRecbto;
              Resposta.NomeArquivo := Inutilizacao.NomeArquivo;
              Resposta.Xml := Inutilizacao.XML_ProcInutNFe;

              MoverStringParaPChar(Resposta.Gerar, sResposta, esTamanho);
              Result := SetRetorno(ErrOK, StrPas(sResposta));
            end
            else
              Result := SetRetornoWebService(SSL.HTTPResultCode, Inutilizacao.XMotivo);
          end;
        end;
      finally
        Resposta.Free;
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_Enviar(ALote: Integer; Imprimir: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: String;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFe_Enviar(' + IntToStr(ALote) + ',' +
                   BoolToStr(Imprimir, 'Imprimir','') + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFe_Enviar', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;

      with NFeDM.ACBrNFe1 do
      begin
        if NotasFiscais.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfNFeCarregadas, [NotasFiscais.Count]))
        else
        begin
          NotasFiscais.Assinar;
          NotasFiscais.Validar;

          if (ALote = 0) then
            WebServices.Enviar.Lote := '1'
          else
            WebServices.Enviar.Lote := IntToStr(ALote);

          if WebServices.Enviar.Executar then
          begin
            Resposta := RespostaEnvio;

            WebServices.Retorno.Recibo := WebServices.Enviar.Recibo;

            if WebServices.Retorno.Executar then
            begin
              Resposta := Resposta + RespostaRetorno;

              MoverStringParaPChar(Resposta, sResposta, esTamanho);
              Result := SetRetorno(ErrOK, StrPas(sResposta));
            end
            else
              Result := SetRetornoWebService(SSL.HTTPResultCode, 'Consultar Recibo');
          end
          else
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'Enviar');
        end;
      end;

      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AChave, AJustificativa, ACNPJ: string;
  Resposta: string;
begin
  try
    VerificarLibInicializada;

    AChave := string(eChave);
    AJustificativa := string(eJustificativa);
    ACNPJ := string(eCNPJ);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFe_Cancelar(' + AChave + ',' + AJustificativa + ',' +
                        ACNPJ + ',' + IntToStr(ALote) + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFe_Cancelar', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;

      if not ValidarChave(AChave) then
        raise EACBrLibException.Create(ErrChaveNFe, Format(SErrChaveInvalida, [AChave]))
      else
        NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave := AChave;

      if not NFeDM.ACBrNFe1.WebServices.Consulta.Executar then
        raise EACBrLibException.Create(ErrConsulta, NFeDM.ACBrNFe1.WebServices.Consulta.Msg);

      NFeDM.ACBrNFe1.EventoNFe.Evento.Clear;

      with NFeDM.ACBrNFe1.EventoNFe.Evento.New do
      begin
        Infevento.CNPJ := ACNPJ;
        if Trim(Infevento.CNPJ) = '' then
          Infevento.CNPJ := copy(OnlyNumber(NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave), 7, 14)
        else
        begin
          if not ValidarCNPJ(ACNPJ) then
            raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJInvalido, [ACNPJ]));
        end;

        Infevento.cOrgao := StrToIntDef(
          copy(OnlyNumber(NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave), 1, 2), 0);
        Infevento.dhEvento := now;
        Infevento.tpEvento := teCancelamento;
        Infevento.chNFe := NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave;
        Infevento.detEvento.nProt := NFeDM.ACBrNFe1.WebServices.Consulta.Protocolo;
        Infevento.detEvento.xJust := AJustificativa;
      end;

      try
        if NFeDM.ACBrNFe1.EnviarEvento(ALote) then
        begin
          Resposta := RespostaCancelamento;

          MoverStringParaPChar(Resposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, StrPas(sResposta));
        end
        else
          Result := SetRetornoWebService(NFeDM.ACBrNFe1.SSL.HTTPResultCode, 'Cancelar');
      except
        raise EACBrLibException.Create(ErrRetorno, NFeDM.ACBrNFe1.WebServices.EnvEvento.EventoRetorno.xMotivo);
      end;

      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_EnviarEvento(idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: String;
  I: Integer;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFe_EnviarEvento(' + IntToStr(idLote) + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFe_EnviarEvento', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;

      with NFeDM.ACBrNFe1 do
      begin
        if EventoNFe.Evento.Count =0 then
          raise EACBrLibException.Create(ErrEnvioEvento, Format(SInfEventosCarregados, [EventoNFe.Evento.Count]))
        else
        begin
          if (idLote = 0) then
            idLote := 1;

          if EnviarEvento(idLote) then
          begin
            Resposta := RespostaEvento;

            for I := 0 to WebServices.EnvEvento.EventoRetorno.retEvento.Count - 1 do
              Resposta := Resposta + RespostaItensEvento(I);

            MoverStringParaPChar(Resposta, sResposta, esTamanho);
            Result := SetRetorno(ErrOK, StrPas(sResposta));
          end
          else
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'Enviar Evento');
        end;
      end;

      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_DistribuicaoDFePorUltNSU(const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AultNSU, ACNPJCPF: string;
  Resposta: string;
  i: Integer;
begin
  try
    VerificarLibInicializada;

    ACNPJCPF := string(eCNPJCPF);
    AultNSU := string(eultNSU);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFe_DistribuicaoDFePorUltNSU(' + IntToStr(AcUFAutor) + ',' +
                     ACNPJCPF + ',' + AultNSU + ',' + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFe_DistribuicaoDFePorUltNSU', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;

      if not ValidarCNPJ(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with NFeDM.ACBrNFe1 do
      begin
        try
          if DistribuicaoDFePorUltNSU(AcUFAutor, ACNPJCPF, AultNSU) then
          begin
            Resposta := RespostaDistribuicaoDFe;

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResNFe(i);

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResEve(i);

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeProEve(i);

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeInfeve(i);

            MoverStringParaPChar(Resposta, sResposta, esTamanho);
            Result := SetRetorno(ErrOK, StrPas(sResposta));
          end
          else
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'DistribuicaoDFePorUltNSU');
        except
          raise EACBrLibException.Create(ErrRetorno, WebServices.DistribuicaoDFe.retDistDFeInt.xMotivo);
        end;
      end;

      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_DistribuicaoDFePorNSU(const AcUFAutor: integer; eCNPJCPF, eNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ANSU, ACNPJCPF: string;
  Resposta: string;
  i: Integer;
begin
  try
    VerificarLibInicializada;

    ACNPJCPF := string(eCNPJCPF);
    ANSU := string(eNSU);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFe_DistribuicaoDFePorNSU(' + IntToStr(AcUFAutor) + ',' +
                     ACNPJCPF + ',' + ANSU + ',' + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFe_DistribuicaoDFePorNSU', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;

      if not ValidarCNPJ(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with NFeDM.ACBrNFe1 do
      begin
        try
          if DistribuicaoDFePorNSU(AcUFAutor, ACNPJCPF, ANSU) then
          begin
            Resposta := RespostaDistribuicaoDFe;

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResNFe(i);

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResEve(i);

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeProEve(i);

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeInfeve(i);

            MoverStringParaPChar(Resposta, sResposta, esTamanho);
            Result := SetRetorno(ErrOK, StrPas(sResposta));
          end
          else
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'DistribuicaoDFePorNSU');
        except
          raise EACBrLibException.Create(ErrRetorno, WebServices.DistribuicaoDFe.retDistDFeInt.xMotivo);
        end;
      end;

      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_DistribuicaoDFePorChave(const AcUFAutor: integer; eCNPJCPF, echNFe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AchNFe, ACNPJCPF: string;
  Resposta: string;
  i: Integer;
begin
  try
    VerificarLibInicializada;

    ACNPJCPF := string(eCNPJCPF);
    AchNFe := string(echNFe);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFe_DistribuicaoDFePorChave(' + IntToStr(AcUFAutor) + ',' +
                     ACNPJCPF + ',' + AchNFe + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFe_DistribuicaoDFePorChave', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;

      if not ValidarCNPJ(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      if not ValidarChave(AchNFe) then
        raise EACBrLibException.Create(ErrChaveNFe, Format(SErrChaveInvalida, [AchNFe]));

      with NFeDM.ACBrNFe1 do
      begin
        try
          if DistribuicaoDFePorChaveNFe(AcUFAutor, ACNPJCPF, AchNFe) then
          begin
            Resposta := RespostaDistribuicaoDFe;

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResNFe(i);

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResEve(i);

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeProEve(i);

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeInfeve(i);

            MoverStringParaPChar(Resposta, sResposta, esTamanho);
            Result := SetRetorno(ErrOK, StrPas(sResposta));
          end
          else
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'DistribuicaoDFePorChaveNFe');
        except
          raise EACBrLibException.Create(ErrRetorno, WebServices.DistribuicaoDFe.retDistDFeInt.xMotivo);
        end;
      end;

      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_EnviarEmail(const ePara, eChaveNFe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  APara, AChaveNFe, AAssunto, ACC, AAnexos, AMensagem: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
  Resposta: TLibNFeResposta;
begin
  try
    VerificarLibInicializada;

    APara := string(ePara);
    AChaveNFe := string(eChaveNFe);
    AAssunto := string(eAssunto);
    ACC := string(eCC);
    AAnexos := string(eAnexos);
    AMensagem := string(eMensagem);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFe_EnviarEmail(' + APara + ',' + AChaveNFe + ',' +
         BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' + ACC + ',' +
         AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFe_EnviarEmail', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        with NFeDM.ACBrNFe1 do
        begin
          EhArquivo := StringEhArquivo(AChaveNFe);

          if EhArquivo then
            VerificarArquivoExiste(AChaveNFe);

          if EhArquivo then
            NotasFiscais.LoadFromFile(AchaveNFe);

          if NotasFiscais.Count = 0 then
            raise EACBrLibException.Create(ErrEnvio, Format(SInfNFeCarregadas, [NotasFiscais.Count]))
          else
          begin
            slMensagemEmail := TStringList.Create;
            slCC := TStringList.Create;
            slAnexos := TStringList.Create;
            Resposta := TLibNFeResposta.Create('EnviaEmail', pLib.Config.TipoResposta);
            try
              with mail do
              begin
                slMensagemEmail.DelimitedText:= sLineBreak;
                slMensagemEmail.Text := StringReplace(AMensagem, ';', sLineBreak, [rfReplaceAll]);

                slCC.DelimitedText:= sLineBreak;
                slCC.Text := StringReplace(ACC, ';', sLineBreak, [rfReplaceAll]);

                slAnexos.DelimitedText := sLineBreak;
                slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

                try
                  NFeDM.ACBrNFe1.NotasFiscais.Items[0].EnviarEmail(
                    APara,
                    AAssunto,
                    slMensagemEmail,
                    AEnviaPDF, // Enviar PDF junto
                    slCC,      // Lista com emails que serão enviado cópias - TStrings
                    slAnexos); // Lista de slAnexos - TStrings

                  Resposta.Msg := 'Email enviado com sucesso';
                  Result := SetRetorno(ErrOK, Resposta.Gerar);
                except
                  on E: Exception do
                    raise EACBrLibException.Create(ErrRetorno, 'Erro ao enviar email' + sLineBreak + E.Message);
                end;
              end;
            finally
              Resposta.Free;
              slCC.Free;
              slAnexos.Free;
              slMensagemEmail.Free;
            end;
          end;
        end;
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_EnviarEmailEvento(const ePara, eChaveEvento, eChaveNFe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  APara, AChaveEvento, AChaveNFe, AAssunto, ACC, AAnexos, AMensagem,
  ArqPDF: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
  Resposta: TLibNFeResposta;
begin
  try
    VerificarLibInicializada;

    APara := string(ePara);
    AChaveEvento := string(eChaveEvento);
    AChaveNFe := string(eChaveNFe);
    AAssunto := string(eAssunto);
    ACC := string(eCC);
    AAnexos := string(eAnexos);
    AMensagem := string(eMensagem);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFe_EnviarEmailEvento(' + APara + ',' + AChaveEvento + ',' +
         AChaveNFe + ',' + BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' +
         ACC + ',' + AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFe_EnviarEmailEvento', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        with NFeDM.ACBrNFe1 do
        begin
          EventoNFe.Evento.Clear;
          NotasFiscais.Clear;

          EhArquivo := StringEhArquivo(AChaveEvento);

          if EhArquivo then
            VerificarArquivoExiste(AChaveEvento);

          if EhArquivo then
            EventoNFe.LerXML(AChaveEvento);

          EhArquivo := StringEhArquivo(AChaveNFe);

          if EhArquivo then
            VerificarArquivoExiste(AChaveNFe);

          if EhArquivo then
            NotasFiscais.LoadFromFile(AchaveNFe);

          if EventoNFe.Evento.Count = 0 then
            raise EACBrLibException.Create(ErrEnvio,
                    Format(SInfEventosCarregados, [EventoNFe.Evento.Count]))
          else
          begin
            slMensagemEmail := TStringList.Create;
            slCC := TStringList.Create;
            slAnexos := TStringList.Create;
            Resposta := TLibNFeResposta.Create('EnviaEmail', pLib.Config.TipoResposta);
            try
              if AEnviaPDF then
              begin
                try
                  ImprimirEventoPDF;

                  ArqPDF := OnlyNumber(EventoNFe.Evento[0].Infevento.id);
                  ArqPDF := PathWithDelim(DANFe.PathPDF)+ArqPDF+'-procEventoNFe.pdf';
                except
                  raise EACBrLibException.Create(ErrRetorno, 'Erro ao criar o arquivo PDF');
                end;
              end;

              with mail do
              begin
                slMensagemEmail.DelimitedText:= sLineBreak;
                slMensagemEmail.Text := StringReplace(AMensagem, ';', sLineBreak, [rfReplaceAll]);

                slCC.DelimitedText:= sLineBreak;
                slCC.Text := StringReplace(ACC, ';', sLineBreak, [rfReplaceAll]);

                slAnexos.DelimitedText := sLineBreak;
                slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

                slAnexos.Add(AChaveEvento);

                if AEnviaPDF then
                  slAnexos.Add(ArqPDF);

                try
                  NFeDM.ACBrNFe1.EnviarEmail(
                    APara,
                    AAssunto,
                    slMensagemEmail,
                    slCC,      // Lista com emails que serão enviado cópias - TStrings
                    slAnexos); // Lista de slAnexos - TStrings

                  Resposta.Msg := 'Email enviado com sucesso';
                  Result := SetRetorno(ErrOK, Resposta.Gerar);
                except
                  on E: Exception do
                    raise EACBrLibException.Create(ErrRetorno, 'Erro ao enviar email' + sLineBreak + E.Message);
                end;
              end;
            finally
              Resposta.Free;
              slCC.Free;
              slAnexos.Free;
              slMensagemEmail.Free;
            end;
          end;
        end;
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_Imprimir(const cImpressora: PChar; nNumCopias: Integer; const cProtocolo,
  bMostrarPreview, cMarcaDagua, bViaConsumidor, bSimplificado: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resposta: TLibNFeResposta;
  Impressora, Protocolo,
  MostrarPreview, MarcaDagua,
  ViaConsumidor, Simplificado: String;
begin
  try
    VerificarLibInicializada;

    Impressora := String(cImpressora);
    Protocolo := String(cProtocolo);
    MostrarPreview := String(bMostrarPreview);
    MarcaDagua := String(cMarcaDagua);
    ViaConsumidor := String(bViaConsumidor);
    Simplificado := String(bSimplificado);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFe_Imprimir(' + Impressora + ',' + IntToStr(nNumCopias) + ','
        + Protocolo + ',' + MostrarPreview + ',' + MarcaDagua + ','
        + ViaConsumidor + ',' + Simplificado + ')', logCompleto, True)
    else
      pLib.GravarLog('NFe_Imprimir', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      Resposta := TLibNFeResposta.Create('Imprimir', pLib.Config.TipoResposta);
      try
        NFeDM.ConfigurarImpressao(Impressora, False, Protocolo, MostrarPreview,
          MarcaDagua, ViaConsumidor, Simplificado);
        NFeDM.ACBrNFe1.NotasFiscais.Imprimir;
        Resposta.Msg := 'Danfe Impresso com sucesso';
        Result := SetRetorno(ErrOK, Resposta.Gerar);
      finally
        Resposta.Free;
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resposta: TLibNFeResposta;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFe_ImprimirPDF', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      Resposta := TLibNFeResposta.Create('Imprimir', pLib.Config.TipoResposta);
      try
        NFeDM.ConfigurarImpressao('', true);
        NFeDM.ACBrNFe1.NotasFiscais.ImprimirPDF;

        Resposta.Msg := NFeDM.ACBrNFe1.DANFE.ArquivoPDF;
        Result := SetRetorno(ErrOK, Resposta.Gerar);
      finally
        Resposta.Free;
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_ImprimirEvento(const eChaveNFe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AChaveNFe: string;
  AChaveEvento: string;
  Resposta: TLibNFeResposta;
begin
  try
    VerificarLibInicializada;

    AChaveNFe := string(eChaveNFe);
    AChaveEvento := string(eChaveEvento);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFe_ImprimirEvento(' + AChaveNFe + ',' + AChaveEvento + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFe_ImprimirEvento', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      Resposta := TLibNFeResposta.Create('Imprimir', pLib.Config.TipoResposta);
      try
        EhArquivo := StringEhArquivo(AChaveNFe);

        if EhArquivo then
          VerificarArquivoExiste(AChaveNFe);

        if EhArquivo then
          NFeDM.ACBrNFe1.NotasFiscais.LoadFromFile(AchaveNFe);

        EhArquivo := StringEhArquivo(AChaveEvento);

        if EhArquivo then
          VerificarArquivoExiste(AChaveEvento);

        if EhArquivo then
          NFeDM.ACBrNFe1.EventoNFe.LerXML(AChaveEvento);

        NFeDM.ConfigurarImpressao();
        NFeDM.ACBrNFe1.ImprimirEvento;

        Resposta.Msg := 'Danfe Impresso com sucesso';
        Result := SetRetorno(ErrOK, Resposta.Gerar);
      finally
        Resposta.Free;
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_ImprimirEventoPDF(const eChaveNFe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AChaveNFe: string;
  AChaveEvento: string;
  Resposta: TLibNFeResposta;
begin
  try
    VerificarLibInicializada;

    AChaveNFe := string(eChaveNFe);
    AChaveEvento := string(eChaveEvento);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFe_ImprimirEventoPDF(' + AChaveNFe + ',' + AChaveEvento + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFe_ImprimirEventoPDF', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      Resposta := TLibNFeResposta.Create('Imprimir', pLib.Config.TipoResposta);
      try
        EhArquivo := StringEhArquivo(AChaveNFe);

        if EhArquivo then
          VerificarArquivoExiste(AChaveNFe);

        if EhArquivo then
          NFeDM.ACBrNFe1.NotasFiscais.LoadFromFile(AchaveNFe);

        EhArquivo := StringEhArquivo(AChaveEvento);

        if EhArquivo then
          VerificarArquivoExiste(AChaveEvento);

        if EhArquivo then
          NFeDM.ACBrNFe1.EventoNFe.LerXML(AChaveEvento);

        NFeDM.ConfigurarImpressao('', false);
        NFeDM.ACBrNFe1.ImprimirEventoPDF;

        Resposta.Msg := NFeDM.ACBrNFe1.DANFE.ArquivoPDF;
        Result := SetRetorno(ErrOK, Resposta.Gerar);
      finally
        Resposta.Free;
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_ImprimirInutilizacao(const eChave: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AChave: string;
  Resposta: TLibNFeResposta;
begin
  try
    VerificarLibInicializada;

    AChave := string(eChave);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFe_ImprimirInutilizacao(' + AChave + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFe_ImprimirInutilizacao', logNormal);

    EhArquivo := StringEhArquivo(AChave);

    if EhArquivo then
      VerificarArquivoExiste(AChave);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      Resposta := TLibNFeResposta.Create('Imprimir', pLib.Config.TipoResposta);
      try

        if EhArquivo then
          NFeDM.ACBrNFe1.InutNFe.LerXML(AChave);

        NFeDM.ConfigurarImpressao();
        NFeDM.ACBrNFe1.ImprimirInutilizacao;

        Resposta.Msg := 'Danfe Impresso com sucesso';
        Result := SetRetorno(ErrOK, Resposta.Gerar);
      finally
        Resposta.Free;
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_ImprimirInutilizacaoPDF(const eChave: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AChave: string;
  Resposta: TLibNFeResposta;
begin
  try
    VerificarLibInicializada;

    AChave := string(eChave);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFe_ImprimirInutilizacaoPDF(' + AChave + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFe_ImprimirInutilizacaoPDF', logNormal);

    EhArquivo := StringEhArquivo(AChave);

    if EhArquivo then
      VerificarArquivoExiste(AChave);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      Resposta := TLibNFeResposta.Create('Imprimir', pLib.Config.TipoResposta);
      try
        if EhArquivo then
          NFeDM.ACBrNFe1.InutNFe.LerXML(AChave);

        NFeDM.ConfigurarImpressao('', false);
        NFeDM.ACBrNFe1.ImprimirInutilizacaoPDF;

        Resposta.Msg := NFeDM.ACBrNFe1.DANFE.ArquivoPDF;
        Result := SetRetorno(ErrOK, Resposta.Gerar);
      finally
        Resposta.Free;
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

{%endregion}

initialization
  pLibClass := TACBrLibNFe;

end.
