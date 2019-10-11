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

unit ACBrLibBPeClass;

interface

uses
  Classes, SysUtils, Forms,
  ACBrLibComum, ACBrLibBPeDataModule;

type

  { TACBrLibBPe }

  TACBrLibBPe = class(TACBrLib)
  private
    FBPeDM: TLibBPeDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property BPeDM: TLibBPeDM read FBPeDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function BPe_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region BPe}
function BPe_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_Assinar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_Validar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Servicos}
function RespostaCancelamento: String;
function RespostaDistribuicaoDFe: String;
function RespostaItensDistribuicaoDFeResBPe(ItemID: integer = 0): String;
function RespostaItensDistribuicaoDFeResEve(ItemID: integer = 0): String;
function RespostaItensDistribuicaoDFeProEve(ItemID: integer = 0): String;
function RespostaItensDistribuicaoDFeInfeve(ItemID: integer = 0): String;
function RespostaEnvio: String;
function RespostaRetorno: String;
function RespostaEvento: String;
function RespostaItensEvento(ItemID: integer = 0): String;

function BPe_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_Consultar(const eChaveOuBPe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_Enviar(ALote: Integer; Imprimir: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_EnviarEvento(idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_DistribuicaoDFePorUltNSU(const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_DistribuicaoDFePorNSU(const AcUFAutor: integer; eCNPJCPF, eNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_DistribuicaoDFePorChave(const AcUFAutor: integer; eCNPJCPF, echBPe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function BPe_EnviarEmail(const ePara, eChaveBPe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function BPe_EnviarEmailEvento(const ePara, eChaveEvento, eChaveBPe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function BPe_Imprimir: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_ImprimirEvento(const eChaveBPe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BPe_ImprimirEventoPDF(const eChaveBPe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibBPeConsts, ACBrLibConfig, ACBrLibResposta,
  ACBrLibBPeConfig, ACBrLibBPeRespostas, ACBrBPe, ACBrMail,
  pcnConversao, pcnAuxiliar, blcksock, ACBrUtil;

{ TACBrLibBPe }

constructor TACBrLibBPe.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FBPeDM := TLibBPeDM.Create(nil);
end;

destructor TACBrLibBPe.Destroy;
begin
  FBPeDM.Free;

  inherited Destroy;
end;

procedure TACBrLibBPe.Inicializar;
begin
  GravarLog('TACBrLibBPe.Inicializar', logNormal);

  FBPeDM.CriarACBrMail;
  FBPeDM.CriarACBrPosPrinter;

  GravarLog('TACBrLibBPe.Inicializar - Feito', logParanoico);

  inherited Inicializar;
end;

procedure TACBrLibBPe.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibBPeConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibBPe.Executar;
begin
  inherited Executar;
  FBPeDM.AplicarConfiguracoes;
end;

{%region BPe}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function BPe_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function BPe_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function BPe_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function BPe_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function BPe_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function BPe_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function BPe_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function BPe_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function BPe_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;

{%endregion}

function BPe_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuXml := string(eArquivoOuXML);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('BPe_CarregarXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      pLib.GravarLog('BPe_CarregarXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;
      try
        if EhArquivo then
          BPeDM.ACBrBPe1.Bilhetes.LoadFromFile(ArquivoOuXml)
        else
          BPeDM.ACBrBPe1.Bilhetes.LoadFromString(ArquivoOuXml);

        Result := SetRetornoBPesCarregados(BPeDM.ACBrBPe1.Bilhetes.Count);
      finally
        BPeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ArquivoOuINI: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuINI := string(eArquivoOuINI);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('BPe_CarregarINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      pLib.GravarLog('BPe_CarregarINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;
      try
        BPeDM.ACBrBPe1.Bilhetes.LoadFromIni(ArquivoOuINI);
        Result := SetRetornoBPesCarregados(BPeDM.ACBrBPe1.Bilhetes.Count);
      finally
        BPeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('BPe_LimparLista', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;
      try
        BPeDM.ACBrBPe1.Bilhetes.Clear;
        Result := SetRetornoBPesCarregados(BPeDM.ACBrBPe1.Bilhetes.Count);
      finally
        BPeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_Assinar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('BPe_Assinar', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;
      try
        try
          BPeDM.ACBrBPe1.Bilhetes.Assinar;
        except
          on E: EACBrBPeException do
            Result := SetRetorno(ErrAssinarBPe, E.Message);
        end;
        Result := SetRetornoBPesCarregados(BPeDM.ACBrBPe1.Bilhetes.Count);
      finally
        BPeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_Validar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('BPe_Validar', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;
      try
        try
          BPeDM.ACBrBPe1.Bilhetes.Validar;
        except
          on E: EACBrBPeException do
            Result := SetRetorno(ErrValidacaoBPe, E.Message);
        end;
        Result := SetRetornoBPesCarregados(BPeDM.ACBrBPe1.Bilhetes.Count);
      finally
        BPeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Erros: string;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('BPe_ValidarRegrasdeNegocios', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;
      try
        Erros := '';
        BPeDM.ACBrBPe1.Bilhetes.ValidarRegrasdeNegocios(Erros);
        MoverStringParaPChar(Erros, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        BPeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Erros: string;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('BPe_VerificarAssinatura', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;
      try
        Erros := '';
        BPeDM.ACBrBPe1.Bilhetes.VerificarAssinatura(Erros);
        MoverStringParaPChar(Erros, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        BPeDM.Destravar;
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
  Resp := TCancelamentoResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibBPe(pLib).BPeDM.ACBrBPe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfevento do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cOrgao;
      Resp.ChBPe := chBPe;
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
  Resp := TDistribuicaoDFeResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibBPe(pLib).BPeDM.ACBrBPe1.WebServices.DistribuicaoDFe do
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

function RespostaItensDistribuicaoDFeResBPe(ItemID: integer = 0): String;
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  Resp := TDistribuicaoDFeItemResposta.Create(
    'ResBPe' + Trim(IntToStrZero(ItemID +1, 3)), pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibBPe(pLib).BPeDM.ACBrBPe1.WebServices.DistribuicaoDFe do
    begin
      Resp.NSU := retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.XML := retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := listaArqs[ItemID];
      Resp.schema := SchemaDFeToStr(retDistDFeInt.docZip[ItemID].schema);

      with retDistDFeInt.docZip.Items[ItemID].resDFe do
      begin
        Resp.chBPe := chBPe;
        Resp.CNPJCPF := CNPJCPF;
        Resp.xNome := xNome;
        Resp.IE := IE;
        Resp.dhEmi := dhEmi;
        Resp.vNF := vNF;
        Resp.digVal := digVal;
        Resp.dhRecbto := dhRecbto;
        Resp.cSitBPe := SituacaoDFeToStr(cSitDFe);
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
    'ResEve' + Trim(IntToStrZero(ItemID +1, 3)), pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    // Atualmente o DistribuicaoDFe do CT-e não retorna Resumo de Eventos.
    {
    with fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].resEvento do
    begin
      Resp.NSU := fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.chBPe := chBPe;
      Resp.CNPJCPF := CNPJCPF;
      Resp.dhEvento := dhEvento;
      Resp.tpEvento := TpEventoToStr(tpEvento);
      Resp.xEvento := xEvento;
      Resp.nSeqEvento := nSeqEvento;
      Resp.cOrgao := cOrgao;
      Resp.dhRecbto := dhRecbto;
      Resp.nProt := nProt;
      Resp.XML := fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := fACBrBPe.WebServices.DistribuicaoDFe.listaArqs[ItemID];
      Resp.schema := SchemaBPeToStr(fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[ItemID].schema);

      Result := Resp.Gerar;
    end;
    }
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
    'ProEve' + Trim(IntToStrZero(ItemID +1, 3)), pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibBPe(pLib).BPeDM.ACBrBPe1.WebServices.DistribuicaoDFe do
    begin
      Resp.NSU := retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.XML := retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := listaArqs[ItemID];
      Resp.schema := SchemaDFeToStr(retDistDFeInt.docZip[ItemID].schema);

      with retDistDFeInt.docZip.Items[ItemID].procEvento do
      begin
        Resp.chBPe := chBPe;
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
//          Resp.BPeNProt := BPe.nProt;
//          Resp.BPeChvBPe := BPe.chBPe;
//          Resp.BPeDhemi := BPe.dhEmi;
//          Resp.BPeModal := TpModalToStr(BPe.modal);
//          Resp.BPeDhRebcto := BPe.dhRecbto;
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
    'Infeve' + Trim(IntToStrZero(ItemID +1, 3)), pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibBPe(pLib).BPeDM.ACBrBPe1.WebServices.DistribuicaoDFe do
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
        Resp.chBPe := chBPe;
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
  Resp := TEnvioResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibBPe(pLib).BPeDM.ACBrBPe1.WebServices.Enviar do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.verAplic := verAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := xMotivo;
      Resp.CUF := cUF;
//      Resp.nRec := Recibo;
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
  Resp := TRetornoResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    {
    with TACBrLibBPe(pLib).BPeDM.ACBrBPe1.WebServices.Retorno do
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
    }
  finally
    Resp.Free;
  end;
end;

function RespostaEvento: String;
var
  Resp: TEventoResposta;
begin
  Resp := TEventoResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibBPe(pLib).BPeDM.ACBrBPe1.WebServices.EnvEvento.EventoRetorno do
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
    'EVENTO' + Trim(IntToStrZero(ItemID +1, 3)), pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibBPe(pLib).BPeDM.ACBrBPe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[ItemID].RetInfevento do
    begin
      Resp.Id := Id;
      Resp.tpAmb := TpAmbToStr(tpAmb);
      Resp.verAplic := verAplic;
      Resp.cOrgao := cOrgao;
      Resp.cStat := cStat;
      Resp.xMotivo := xMotivo;
      Resp.chBPe := chBPe;
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

function BPe_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: TStatusServicoResposta;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('BPe_StatusServico', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;
      Resposta := TStatusServicoResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
      try
        with BPeDM.ACBrBPe1 do
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
        BPeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_Consultar(const eChaveOuBPe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ChaveOuBPe: string;
  Resposta: TConsultaBPeResposta;
begin
  try
    VerificarLibInicializada;

    ChaveOuBPe := string(eChaveOuBPe);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('BPe_Consultar(' + ChaveOuBPe + ' )', logCompleto, True)
    else
      pLib.GravarLog('BPe_Consultar', logNormal);

    EhArquivo := StringEhArquivo(ChaveOuBPe);
    if EhArquivo then
      VerificarArquivoExiste(ChaveOuBPe);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;

      if EhArquivo then
        BPeDM.ACBrBPe1.Bilhetes.LoadFromFile(ChaveOuBPe);

      if BPeDM.ACBrBPe1.Bilhetes.Count = 0 then
      begin
        if ValidarChave(ChaveOuBPe) then
          BPeDM.ACBrBPe1.WebServices.Consulta.BPeChave := ChaveOuBPe
        else
          raise EACBrLibException.Create(ErrChaveBPe, Format(SErrChaveInvalida, [ChaveOuBPe]));
      end
      else
        BPeDM.ACBrBPe1.WebServices.Consulta.BPeChave := StringReplace(
          BPeDM.ACBrBPe1.Bilhetes.Items[BPeDM.ACBrBPe1.Bilhetes.Count - 1].BPe.infBPe.ID,
          'BPe','',[rfIgnoreCase]);

      Resposta := TConsultaBPeResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
      try
        with BPeDM.ACBrBPe1 do
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
            Resposta.ChBPe := WebServices.Consulta.BPeChave;
            Resposta.NProt := WebServices.Consulta.Protocolo;
            Resposta.DigVal := WebServices.Consulta.protBPe.digVal;

            MoverStringParaPChar(Resposta.Gerar, sResposta, esTamanho);
            Result := SetRetorno(ErrOK, StrPas(sResposta));
          end
          else
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'Consultar');
        end;
      finally
        Resposta.Free;
        BPeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_Enviar(ALote: Integer; Imprimir: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: String;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('BPe_Enviar(' + IntToStr(ALote) + ',' +
                   BoolToStr(Imprimir, 'Imprimir','') + ' )', logCompleto, True)
    else
      pLib.GravarLog('BPe_Enviar', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;

      with BPeDM.ACBrBPe1 do
      begin
        if Bilhetes.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfBPeCarregados, [Bilhetes.Count]))
        else
        begin
          Bilhetes.Assinar;
          Bilhetes.Validar;

          if (ALote = 0) then
            WebServices.Enviar.Lote := '1'
          else
            WebServices.Enviar.Lote := IntToStr(ALote);

          if WebServices.Enviar.Executar then
          begin
            Resposta := RespostaEnvio;

//            WebServices.Retorno.Recibo := WebServices.Enviar.Recibo;

//            if WebServices.Retorno.Executar then
//            begin
//              Resposta := Resposta + RespostaRetorno;

              MoverStringParaPChar(Resposta, sResposta, esTamanho);
              Result := SetRetorno(ErrOK, StrPas(sResposta));
//            end
//            else
//              Result := SetRetornoWebService(SSL.HTTPResultCode, 'Consultar Recibo');
          end
          else
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'Enviar');
        end;
      end;

      BPeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
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
      pLib.GravarLog('BPe_Cancelar(' + AChave + ',' + AJustificativa + ',' +
                        ACNPJ + ',' + IntToStr(ALote) + ' )', logCompleto, True)
    else
      pLib.GravarLog('BPe_Cancelar', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;

      if not ValidarChave(AChave) then
        raise EACBrLibException.Create(ErrChaveBPe, Format(SErrChaveInvalida, [AChave]))
      else
        BPeDM.ACBrBPe1.WebServices.Consulta.BPeChave := AChave;

      if not BPeDM.ACBrBPe1.WebServices.Consulta.Executar then
        raise EACBrLibException.Create(ErrConsulta, BPeDM.ACBrBPe1.WebServices.Consulta.Msg);

      BPeDM.ACBrBPe1.EventoBPe.Evento.Clear;

      with BPeDM.ACBrBPe1.EventoBPe.Evento.Add do
      begin
        Infevento.CNPJ := ACNPJ;
        if Trim(Infevento.CNPJ) = '' then
          Infevento.CNPJ := copy(OnlyNumber(BPeDM.ACBrBPe1.WebServices.Consulta.BPeChave), 7, 14)
        else
        begin
          if not ValidarCNPJ(ACNPJ) then
            raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJInvalido, [ACNPJ]));
        end;

        Infevento.cOrgao := StrToIntDef(
          copy(OnlyNumber(BPeDM.ACBrBPe1.WebServices.Consulta.BPeChave), 1, 2), 0);
        Infevento.dhEvento := now;
        Infevento.tpEvento := teCancelamento;
        Infevento.chBPe := BPeDM.ACBrBPe1.WebServices.Consulta.BPeChave;
        Infevento.detEvento.nProt := BPeDM.ACBrBPe1.WebServices.Consulta.Protocolo;
        Infevento.detEvento.xJust := AJustificativa;
      end;

      try
        if BPeDM.ACBrBPe1.EnviarEvento(ALote) then
        begin
          Resposta := RespostaCancelamento;

          MoverStringParaPChar(Resposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, StrPas(sResposta));
        end
        else
          Result := SetRetornoWebService(BPeDM.ACBrBPe1.SSL.HTTPResultCode, 'Cancelar');
      except
        raise EACBrLibException.Create(ErrRetorno, BPeDM.ACBrBPe1.WebServices.EnvEvento.EventoRetorno.xMotivo);
      end;

      BPeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_EnviarEvento(idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: String;
  I: Integer;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('BPe_EnviarEvento(' + IntToStr(idLote) + ' )', logCompleto, True)
    else
      pLib.GravarLog('BPe_EnviarEvento', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;

      with BPeDM.ACBrBPe1 do
      begin
        if EventoBPe.Evento.Count =0 then
          raise EACBrLibException.Create(ErrEnvioEvento, Format(SInfEventosCarregados, [EventoBPe.Evento.Count]))
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

      BPeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_DistribuicaoDFePorUltNSU(const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar;
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
      pLib.GravarLog('BPe_DistribuicaoDFePorUltNSU(' + IntToStr(AcUFAutor) + ',' +
                     ACNPJCPF + ',' + AultNSU + ',' + ' )', logCompleto, True)
    else
      pLib.GravarLog('BPe_DistribuicaoDFePorUltNSU', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;

      if not ValidarCNPJ(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with BPeDM.ACBrBPe1 do
      begin
        try
          if DistribuicaoDFePorUltNSU(AcUFAutor, ACNPJCPF, AultNSU) then
          begin
            Resposta := RespostaDistribuicaoDFe;

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResBPe(i);

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

      BPeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_DistribuicaoDFePorNSU(const AcUFAutor: integer; eCNPJCPF, eNSU: PChar;
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
      pLib.GravarLog('BPe_DistribuicaoDFePorNSU(' + IntToStr(AcUFAutor) + ',' +
                     ACNPJCPF + ',' + ANSU + ',' + ' )', logCompleto, True)
    else
      pLib.GravarLog('BPe_DistribuicaoDFePorNSU', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;

      if not ValidarCNPJ(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with BPeDM.ACBrBPe1 do
      begin
        try
          if DistribuicaoDFePorNSU(AcUFAutor, ACNPJCPF, ANSU) then
          begin
            Resposta := RespostaDistribuicaoDFe;

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResBPe(i);

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

      BPeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_DistribuicaoDFePorChave(const AcUFAutor: integer; eCNPJCPF, echBPe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AchBPe, ACNPJCPF: string;
  Resposta: string;
  i: Integer;
begin
  try
    VerificarLibInicializada;

    ACNPJCPF := string(eCNPJCPF);
    AchBPe := string(echBPe);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('BPe_DistribuicaoDFePorChave(' + IntToStr(AcUFAutor) + ',' +
                     ACNPJCPF + ',' + AchBPe + ' )', logCompleto, True)
    else
      pLib.GravarLog('BPe_DistribuicaoDFePorChave', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;

      if not ValidarCNPJ(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      if not ValidarChave(AchBPe) then
        raise EACBrLibException.Create(ErrChaveBPe, Format(SErrChaveInvalida, [AchBPe]));

      with BPeDM.ACBrBPe1 do
      begin
        try
          if DistribuicaoDFePorChaveBPe(AcUFAutor, ACNPJCPF, AchBPe) then
          begin
            Resposta := RespostaDistribuicaoDFe;

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResBPe(i);

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
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'DistribuicaoDFePorChaveBPe');
        except
          raise EACBrLibException.Create(ErrRetorno, WebServices.DistribuicaoDFe.retDistDFeInt.xMotivo);
        end;
      end;

      BPeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_EnviarEmail(const ePara, eChaveBPe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  APara, AChaveBPe, AAssunto, ACC, AAnexos, AMensagem: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
begin
  try
    VerificarLibInicializada;

    APara := string(ePara);
    AChaveBPe := string(eChaveBPe);
    AAssunto := string(eAssunto);
    ACC := string(eCC);
    AAnexos := string(eAnexos);
    AMensagem := string(eMensagem);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('BPe_EnviarEmail(' + APara + ',' + AChaveBPe + ',' +
         BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' + ACC + ',' +
         AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      pLib.GravarLog('BPe_EnviarEmail', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;

      with BPeDM.ACBrBPe1 do
      begin
        EhArquivo := StringEhArquivo(AChaveBPe);

        if EhArquivo then
          VerificarArquivoExiste(AChaveBPe);

        if EhArquivo then
          Bilhetes.LoadFromFile(AchaveBPe);

        if Bilhetes.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfBPeCarregados, [Bilhetes.Count]))
        else
        begin
          slMensagemEmail := TStringList.Create;
          slCC := TStringList.Create;
          slAnexos := TStringList.Create;
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
                BPeDM.ACBrBPe1.Bilhetes.Items[0].EnviarEmail(
                  APara,
                  AAssunto,
                  slMensagemEmail,
                  AEnviaPDF, // Enviar PDF junto
                  slCC,      // Lista com emails que serão enviado cópias - TStrings
                  slAnexos); // Lista de slAnexos - TStrings

                Result := SetRetorno(ErrOK, 'Email enviado com sucesso');
              except
                on E: Exception do
                  raise EACBrLibException.Create(ErrRetorno, 'Erro ao enviar email' + sLineBreak + E.Message);
              end;
            end;
          finally
            slCC.Free;
            slAnexos.Free;
            slMensagemEmail.Free;
          end;
        end;
      end;

      BPeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_EnviarEmailEvento(const ePara, eChaveEvento, eChaveBPe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  APara, AChaveEvento, AChaveBPe, AAssunto, ACC, AAnexos, AMensagem,
  ArqPDF: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
begin
  try
    VerificarLibInicializada;

    APara := string(ePara);
    AChaveEvento := string(eChaveEvento);
    AChaveBPe := string(eChaveBPe);
    AAssunto := string(eAssunto);
    ACC := string(eCC);
    AAnexos := string(eAnexos);
    AMensagem := string(eMensagem);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('BPe_EnviarEmailEvento(' + APara + ',' + AChaveEvento + ',' +
         AChaveBPe + ',' + BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' +
         ACC + ',' + AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      pLib.GravarLog('BPe_EnviarEmailEvento', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;

      with BPeDM.ACBrBPe1 do
      begin
        EventoBPe.Evento.Clear;
        Bilhetes.Clear;

        EhArquivo := StringEhArquivo(AChaveEvento);

        if EhArquivo then
          VerificarArquivoExiste(AChaveEvento);

        if EhArquivo then
          EventoBPe.LerXML(AChaveEvento);

        EhArquivo := StringEhArquivo(AChaveBPe);

        if EhArquivo then
          VerificarArquivoExiste(AChaveBPe);

        if EhArquivo then
          Bilhetes.LoadFromFile(AchaveBPe);

        if EventoBPe.Evento.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio,
                  Format(SInfEventosCarregados, [EventoBPe.Evento.Count]))
        else
        begin
          slMensagemEmail := TStringList.Create;
          slCC := TStringList.Create;
          slAnexos := TStringList.Create;
          try
            if AEnviaPDF then
            begin
              try
                ImprimirEventoPDF;

                ArqPDF := OnlyNumber(EventoBPe.Evento[0].Infevento.id);
                ArqPDF := PathWithDelim(DABPe.PathPDF)+ArqPDF+'-procEventoBPe.pdf';
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
                BPeDM.ACBrBPe1.EnviarEmail(
                  APara,
                  AAssunto,
                  slMensagemEmail,
                  slCC,      // Lista com emails que serão enviado cópias - TStrings
                  slAnexos); // Lista de slAnexos - TStrings

                Result := SetRetorno(ErrOK, 'Email enviado com sucesso');
              except
                on E: Exception do
                  raise EACBrLibException.Create(ErrRetorno, 'Erro ao enviar email' + sLineBreak + E.Message);
              end;
            end;
          finally
            slCC.Free;
            slAnexos.Free;
            slMensagemEmail.Free;
          end;
        end;
      end;

      BPeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_Imprimir: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('BPe_Imprimir', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;
      try
        BPeDM.ACBrBPe1.Bilhetes.Imprimir;
        Result := SetRetornoBPesCarregados(BPeDM.ACBrBPe1.Bilhetes.Count);
      finally
        BPeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('BPe_ImprimirPDF', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;
      try
        BPeDM.ACBrBPe1.Bilhetes.ImprimirPDF;
        Result := SetRetornoBPesCarregados(BPeDM.ACBrBPe1.Bilhetes.Count);
      finally
        BPeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_ImprimirEvento(const eChaveBPe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AChaveBPe: string;
  AChaveEvento: string;
begin
  try
    VerificarLibInicializada;

    AChaveBPe := string(eChaveBPe);
    AChaveEvento := string(eChaveEvento);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('BPe_ImprimirEvento(' + AChaveBPe + ',' + AChaveEvento + ' )', logCompleto, True)
    else
      pLib.GravarLog('BPe_ImprimirEvento', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;

      EhArquivo := StringEhArquivo(AChaveBPe);

      if EhArquivo then
        VerificarArquivoExiste(AChaveBPe);

      if EhArquivo then
        BPeDM.ACBrBPe1.Bilhetes.LoadFromFile(AchaveBPe);

      EhArquivo := StringEhArquivo(AChaveEvento);

      if EhArquivo then
        VerificarArquivoExiste(AChaveEvento);

      if EhArquivo then
        BPeDM.ACBrBPe1.EventoBPe.LerXML(AChaveEvento);

      BPeDM.ACBrBPe1.ImprimirEvento;

      Result := SetRetorno(ErrOK);
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BPe_ImprimirEventoPDF(const eChaveBPe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AChaveBPe: string;
  AChaveEvento: string;
begin
  try
    VerificarLibInicializada;

    AChaveBPe := string(eChaveBPe);
    AChaveEvento := string(eChaveEvento);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('BPe_ImprimirEventoPDF(' + AChaveBPe + ',' + AChaveEvento + ' )', logCompleto, True)
    else
      pLib.GravarLog('BPe_ImprimirEventoPDF', logNormal);

    with TACBrLibBPe(pLib) do
    begin
      BPeDM.Travar;

      EhArquivo := StringEhArquivo(AChaveBPe);

      if EhArquivo then
        VerificarArquivoExiste(AChaveBPe);

      if EhArquivo then
        BPeDM.ACBrBPe1.Bilhetes.LoadFromFile(AchaveBPe);

      EhArquivo := StringEhArquivo(AChaveEvento);

      if EhArquivo then
        VerificarArquivoExiste(AChaveEvento);

      if EhArquivo then
        BPeDM.ACBrBPe1.EventoBPe.LerXML(AChaveEvento);

      BPeDM.ACBrBPe1.ImprimirEventoPDF;

      Result := SetRetorno(ErrOK);
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
  pLibClass := TACBrLibBPe;

end.
