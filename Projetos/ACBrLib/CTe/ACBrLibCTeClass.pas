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

unit ACBrLibCTeClass;

interface

uses
  Classes, SysUtils, Forms, ACBrLibMailImport,
  ACBrLibComum, ACBrLibCTeDataModule;

type

  { TACBrLibCTe }

  TACBrLibCTe = class(TACBrLib)
  private
    FCTeDM: TLibCTeDM;
    FLibMail: TACBrLibMail;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property CTeDM: TLibCTeDM read FCTeDM;
    property LibMail: TACBrLibMail read FLibMail;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function CTe_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region CTe}
function CTe_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_Assinar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_Validar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Servicos}
function RespostaCancelamento: String;
function RespostaDistribuicaoDFe: String;
function RespostaItensDistribuicaoDFeResCTe(ItemID: integer = 0): String;
function RespostaItensDistribuicaoDFeResEve(ItemID: integer = 0): String;
function RespostaItensDistribuicaoDFeProEve(ItemID: integer = 0): String;
function RespostaItensDistribuicaoDFeInfeve(ItemID: integer = 0): String;
function RespostaEnvio: String;
function RespostaRetorno: String;
function RespostaEvento: String;
function RespostaItensEvento(ItemID: integer = 0): String;

function CTe_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_Consultar(const eChaveOuCTe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_Inutilizar(const ACNPJ, AJustificativa: PChar;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_Enviar(ALote: Integer; Imprimir: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_EnviarEvento(idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_DistribuicaoDFePorUltNSU(const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_DistribuicaoDFePorNSU(const AcUFAutor: integer; eCNPJCPF, eNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_DistribuicaoDFePorChave(const AcUFAutor: integer; eCNPJCPF, echCTe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function CTe_EnviarEmail(const ePara, eChaveCTe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function CTe_EnviarEmailEvento(const ePara, eChaveEvento, eChaveCTe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function CTe_Imprimir: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_ImprimirEvento(const eChaveCTe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_ImprimirEventoPDF(const eChaveCTe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_ImprimirInutilizacao(const eChave: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTe_ImprimirInutilizacaoPDF(const eChave: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibCTeConsts, ACBrLibConfig, ACBrLibResposta,
  ACBrLibCTeConfig, ACBrLibCTeRespostas, ACBrCTe, ACBrMail,
  pcnConversao, pcnAuxiliar, pcteConversaoCTe, blcksock, ACBrUtil;

{ TACBrLibCTe }

constructor TACBrLibCTe.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibCTeNome;
  fpVersao := CLibCTeVersao;

  FCTeDM := TLibCTeDM.Create(nil);
end;

destructor TACBrLibCTe.Destroy;
begin
  FCTeDM.Free;
  if FLibMail <> nil then
    FLibMail.Free;

  inherited Destroy;
end;

procedure TACBrLibCTe.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibCTe.Inicializar - Inicializando Mail', logParanoico);

  if FileExists(CACBrMailLIBName) then
  begin
    FLibMail := TACBrLibMail.Create(pLib.Config.NomeArquivo, pLib.Config.ChaveCrypt);
    FCTeDM.ACBrMail1 := FLibMail.GetMail;
  end
  else
  begin
    FCTeDM.ACBrMail1 := TACBrMail.Create(FCTeDM);
  end;

  FCTeDM.ACBrCTe1.MAIL := FCTeDM.ACBrMail1;

  GravarLog('TACBrLibCTe.Inicializar - Inicializando Mail Feito', logParanoico);

  GravarLog('TACBrLibCTe.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibCTe.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibCTeConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibCTe.Executar;
begin
  inherited Executar;
  FCTeDM.AplicarConfiguracoes;
end;

{%region CTe}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function CTe_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function CTe_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function CTe_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function CTe_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function CTe_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function CTe_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function CTe_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function CTe_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function CTe_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;

{%endregion}

function CTe_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuXml := string(eArquivoOuXML);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CTe_CarregarXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_CarregarXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;
      try
        if EhArquivo then
          CTeDM.ACBrCTe1.Conhecimentos.LoadFromFile(ArquivoOuXml)
        else
          CTeDM.ACBrCTe1.Conhecimentos.LoadFromString(ArquivoOuXml);

        Result := SetRetornoCTesCarregados(CTeDM.ACBrCTe1.Conhecimentos.Count);
      finally
        CTeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ArquivoOuINI: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuINI := string(eArquivoOuINI);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CTe_CarregarINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_CarregarINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;
      try
        CTeDM.ACBrCTe1.Conhecimentos.LoadFromIni(ArquivoOuINI);
        Result := SetRetornoCTesCarregados(CTeDM.ACBrCTe1.Conhecimentos.Count);
      finally
        CTeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('CTe_LimparLista', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;
      try
        CTeDM.ACBrCTe1.Conhecimentos.Clear;
        Result := SetRetornoCTesCarregados(CTeDM.ACBrCTe1.Conhecimentos.Count);
      finally
        CTeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_Assinar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('CTe_Assinar', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;
      try
        try
          CTeDM.ACBrCTe1.Conhecimentos.Assinar;
        except
          on E: EACBrCTeException do
            Result := SetRetorno(ErrAssinarCTe, E.Message);
        end;
        Result := SetRetornoCTesCarregados(CTeDM.ACBrCTe1.Conhecimentos.Count);
      finally
        CTeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_Validar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('CTe_Validar', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;
      try
        try
          CTeDM.ACBrCTe1.Conhecimentos.Validar;
        except
          on E: EACBrCTeException do
            Result := SetRetorno(ErrValidacaoCTe, E.Message);
        end;
        Result := SetRetornoCTesCarregados(CTeDM.ACBrCTe1.Conhecimentos.Count);
      finally
        CTeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Erros: string;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('CTe_ValidarRegrasdeNegocios', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;
      try
        Erros := '';
        CTeDM.ACBrCTe1.Conhecimentos.ValidarRegrasdeNegocios(Erros);
        MoverStringParaPChar(Erros, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        CTeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Erros: string;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('CTe_VerificarAssinatura', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;
      try
        Erros := '';
        CTeDM.ACBrCTe1.Conhecimentos.VerificarAssinatura(Erros);
        MoverStringParaPChar(Erros, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        CTeDM.Destravar;
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
    with TACBrLibCTe(pLib).CTeDM.ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfevento do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cOrgao;
      Resp.ChCTe := chCTe;
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
    with TACBrLibCTe(pLib).CTeDM.ACBrCTe1.WebServices.DistribuicaoDFe do
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

function RespostaItensDistribuicaoDFeResCTe(ItemID: integer = 0): String;
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  Resp := TDistribuicaoDFeItemResposta.Create(
    'ResCTe' + Trim(IntToStrZero(ItemID +1, 3)), resINI);
  try
    with TACBrLibCTe(pLib).CTeDM.ACBrCTe1.WebServices.DistribuicaoDFe do
    begin
      Resp.NSU := retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.XML := retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := listaArqs[ItemID];
      Resp.schema := SchemaCTeToStr(retDistDFeInt.docZip[ItemID].schema);

      with retDistDFeInt.docZip.Items[ItemID].resCTe do
      begin
        Resp.chCTe := chCTe;
        Resp.CNPJCPF := CNPJCPF;
        Resp.xNome := xNome;
        Resp.IE := IE;
        Resp.dhEmi := dhEmi;
        Resp.vNF := vNF;
        Resp.digVal := digVal;
        Resp.dhRecbto := dhRecbto;
        Resp.cSitCTe := SituacaoDFeToStr(cSitCTe);
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
    {
    with fACBrCTe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].resEvento do
    begin
      Resp.NSU := fACBrCTe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.chCTe := chCTe;
      Resp.CNPJCPF := CNPJCPF;
      Resp.dhEvento := dhEvento;
      Resp.tpEvento := TpEventoToStr(tpEvento);
      Resp.xEvento := xEvento;
      Resp.nSeqEvento := nSeqEvento;
      Resp.cOrgao := cOrgao;
      Resp.dhRecbto := dhRecbto;
      Resp.nProt := nProt;
      Resp.XML := fACBrCTe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := fACBrCTe.WebServices.DistribuicaoDFe.listaArqs[ItemID];
      Resp.schema := SchemaCTeToStr(fACBrCTe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[ItemID].schema);

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
    'ProEve' + Trim(IntToStrZero(ItemID +1, 3)), resINI);
  try
    with TACBrLibCTe(pLib).CTeDM.ACBrCTe1.WebServices.DistribuicaoDFe do
    begin
      Resp.NSU := retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.XML := retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := listaArqs[ItemID];
      Resp.schema := SchemaCTeToStr(retDistDFeInt.docZip[ItemID].schema);

      with retDistDFeInt.docZip.Items[ItemID].procEvento do
      begin
        Resp.chCTe := chCTe;
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
          Resp.cteChvCte := CTe.chCTe;
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
    with TACBrLibCTe(pLib).CTeDM.ACBrCTe1.WebServices.DistribuicaoDFe do
    begin
      Resp.XML := retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := listaArqs[ItemID];
      Resp.schema := SchemaCTeToStr(retDistDFeInt.docZip[ItemID].schema);

      with retDistDFeInt.docZip.Items[ItemID].procEvento.RetInfevento do
      begin
        Resp.Id := Id;
        Resp.VerAplic := VerAplic;
        Resp.tpAmb := TpAmbToStr(tpAmb);
        Resp.cOrgao := cOrgao;
        Resp.chCTe := chCTe;
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
    with TACBrLibCTe(pLib).CTeDM.ACBrCTe1.WebServices.Enviar do
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
    with TACBrLibCTe(pLib).CTeDM.ACBrCTe1.WebServices.Retorno do
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
    with TACBrLibCTe(pLib).CTeDM.ACBrCTe1.WebServices.EnvEvento.EventoRetorno do
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
    with TACBrLibCTe(pLib).CTeDM.ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[ItemID].RetInfevento do
    begin
      Resp.Id := Id;
      Resp.tpAmb := TpAmbToStr(tpAmb);
      Resp.verAplic := verAplic;
      Resp.cOrgao := cOrgao;
      Resp.cStat := cStat;
      Resp.xMotivo := xMotivo;
      Resp.chCTe := chCTe;
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

function CTe_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: TStatusServicoResposta;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('CTe_StatusServico', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;
      Resposta := TStatusServicoResposta.Create(pLib.Config.TipoResposta);
      try
        with CTeDM.ACBrCTe1 do
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
        CTeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_Consultar(const eChaveOuCTe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ChaveOuCTe: string;
  Resposta: TConsultaCTeResposta;
begin
  try
    VerificarLibInicializada;

    ChaveOuCTe := string(eChaveOuCTe);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CTe_Consultar(' + ChaveOuCTe + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_Consultar', logNormal);

    EhArquivo := StringEhArquivo(ChaveOuCTe);
    if EhArquivo then
      VerificarArquivoExiste(ChaveOuCTe);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;

      if EhArquivo then
        CTeDM.ACBrCTe1.Conhecimentos.LoadFromFile(ChaveOuCTe);

      if CTeDM.ACBrCTe1.Conhecimentos.Count = 0 then
      begin
        if ValidarChave(ChaveOuCTe) then
          CTeDM.ACBrCTe1.WebServices.Consulta.CTeChave := ChaveOuCTe
        else
          raise EACBrLibException.Create(ErrChaveCTe, Format(SErrChaveInvalida, [ChaveOuCTe]));
      end
      else
        CTeDM.ACBrCTe1.WebServices.Consulta.CTeChave := StringReplace(
          CTeDM.ACBrCTe1.Conhecimentos.Items[CTeDM.ACBrCTe1.Conhecimentos.Count - 1].CTe.infCTe.ID,
          'CTe','',[rfIgnoreCase]);

      Resposta := TConsultaCTeResposta.Create(pLib.Config.TipoResposta);
      try
        with CTeDM.ACBrCTe1 do
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
            Resposta.ChCTe := WebServices.Consulta.CTeChave;
            Resposta.NProt := WebServices.Consulta.Protocolo;
            Resposta.DigVal := WebServices.Consulta.protCTe.digVal;

            MoverStringParaPChar(Resposta.Gerar, sResposta, esTamanho);
            Result := SetRetorno(ErrOK, StrPas(sResposta));
          end
          else
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'Consultar');
        end;
      finally
        Resposta.Free;
        CTeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_Inutilizar(const ACNPJ, AJustificativa: PChar;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: TInutilizarCTeResposta;
  CNPJ, Justificativa: string;
begin
  try
    VerificarLibInicializada;

    Justificativa := string(AJustificativa);
    CNPJ := string(ACNPJ);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CTe_Inutilizar(' + CNPJ + ',' + Justificativa + ',' + IntToStr(Ano) + ',' +
        IntToStr(modelo) + ',' + IntToStr(Serie) +  ',' + IntToStr(NumeroInicial) +  ',' +
        IntToStr(NumeroFinal) + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_Inutilizar', logNormal);

    CNPJ := OnlyNumber(CNPJ);

    if not ValidarCNPJ(CNPJ) then
      raise EACBrLibException.Create(ErrChaveCTe, Format(SErrCNPJInvalido, [ACNPJ]));

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;
      Resposta := TInutilizarCTeResposta.Create(pLib.Config.TipoResposta);
      try
        with CTeDM.ACBrCTe1 do
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
              Resposta.Xml := Inutilizacao.XML_ProcInutCTe;

              MoverStringParaPChar(Resposta.Gerar, sResposta, esTamanho);
              Result := SetRetorno(ErrOK, StrPas(sResposta));
            end
            else
              Result := SetRetornoWebService(SSL.HTTPResultCode, 'Inutilizar');
          end;
        end;
      finally
        Resposta.Free;
        CTeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_Enviar(ALote: Integer; Imprimir: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: String;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CTe_Enviar(' + IntToStr(ALote) + ',' +
                   BoolToStr(Imprimir, 'Imprimir','') + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_Enviar', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;

      with CTeDM.ACBrCTe1 do
      begin
        if Conhecimentos.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfCTeCarregados, [Conhecimentos.Count]))
        else
        begin
          Conhecimentos.Assinar;
          Conhecimentos.Validar;

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

      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
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
      pLib.GravarLog('CTe_Cancelar(' + AChave + ',' + AJustificativa + ',' +
                        ACNPJ + ',' + IntToStr(ALote) + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_Cancelar', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;

      if not ValidarChave(AChave) then
        raise EACBrLibException.Create(ErrChaveCTe, Format(SErrChaveInvalida, [AChave]))
      else
        CTeDM.ACBrCTe1.WebServices.Consulta.CTeChave := AChave;

      if not CTeDM.ACBrCTe1.WebServices.Consulta.Executar then
        raise EACBrLibException.Create(ErrConsulta, CTeDM.ACBrCTe1.WebServices.Consulta.Msg);

      CTeDM.ACBrCTe1.EventoCTe.Evento.Clear;

      with CTeDM.ACBrCTe1.EventoCTe.Evento.Add do
      begin
        Infevento.CNPJ := ACNPJ;
        if Trim(Infevento.CNPJ) = '' then
          Infevento.CNPJ := copy(OnlyNumber(CTeDM.ACBrCTe1.WebServices.Consulta.CTeChave), 7, 14)
        else
        begin
          if not ValidarCNPJ(ACNPJ) then
            raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJInvalido, [ACNPJ]));
        end;

        Infevento.cOrgao := StrToIntDef(
          copy(OnlyNumber(CTeDM.ACBrCTe1.WebServices.Consulta.CTeChave), 1, 2), 0);
        Infevento.dhEvento := now;
        Infevento.tpEvento := teCancelamento;
        Infevento.chCTe := CTeDM.ACBrCTe1.WebServices.Consulta.CTeChave;
        Infevento.detEvento.nProt := CTeDM.ACBrCTe1.WebServices.Consulta.Protocolo;
        Infevento.detEvento.xJust := AJustificativa;
      end;

      try
        if CTeDM.ACBrCTe1.EnviarEvento(ALote) then
        begin
          Resposta := RespostaCancelamento;

          MoverStringParaPChar(Resposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, StrPas(sResposta));
        end
        else
          Result := SetRetornoWebService(CTeDM.ACBrCTe1.SSL.HTTPResultCode, 'Cancelar');
      except
        raise EACBrLibException.Create(ErrRetorno, CTeDM.ACBrCTe1.WebServices.EnvEvento.EventoRetorno.xMotivo);
      end;

      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_EnviarEvento(idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: String;
  I: Integer;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CTe_EnviarEvento(' + IntToStr(idLote) + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_EnviarEvento', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;

      with CTeDM.ACBrCTe1 do
      begin
        if EventoCTe.Evento.Count =0 then
          raise EACBrLibException.Create(ErrEnvioEvento, Format(SInfEventosCarregados, [EventoCTe.Evento.Count]))
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

      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_DistribuicaoDFePorUltNSU(const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar;
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
      pLib.GravarLog('CTe_DistribuicaoDFePorUltNSU(' + IntToStr(AcUFAutor) + ',' +
                     ACNPJCPF + ',' + AultNSU + ',' + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_DistribuicaoDFePorUltNSU', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;

      if not ValidarCNPJ(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with CTeDM.ACBrCTe1 do
      begin
        try
          if DistribuicaoDFePorUltNSU(AcUFAutor, ACNPJCPF, AultNSU) then
          begin
            Resposta := RespostaDistribuicaoDFe;

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResCTe(i);

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

      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_DistribuicaoDFePorNSU(const AcUFAutor: integer; eCNPJCPF, eNSU: PChar;
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
      pLib.GravarLog('CTe_DistribuicaoDFePorNSU(' + IntToStr(AcUFAutor) + ',' +
                     ACNPJCPF + ',' + ANSU + ',' + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_DistribuicaoDFePorNSU', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;

      if not ValidarCNPJ(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with CTeDM.ACBrCTe1 do
      begin
        try
          if DistribuicaoDFePorNSU(AcUFAutor, ACNPJCPF, ANSU) then
          begin
            Resposta := RespostaDistribuicaoDFe;

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResCTe(i);

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

      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_DistribuicaoDFePorChave(const AcUFAutor: integer; eCNPJCPF, echCTe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AchCTe, ACNPJCPF: string;
  Resposta: string;
  i: Integer;
begin
  try
    VerificarLibInicializada;

    ACNPJCPF := string(eCNPJCPF);
    AchCTe := string(echCTe);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CTe_DistribuicaoDFePorChave(' + IntToStr(AcUFAutor) + ',' +
                     ACNPJCPF + ',' + AchCTe + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_DistribuicaoDFePorChave', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;

      if not ValidarCNPJ(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      if not ValidarChave(AchCTe) then
        raise EACBrLibException.Create(ErrChaveCTe, Format(SErrChaveInvalida, [AchCTe]));

      with CTeDM.ACBrCTe1 do
      begin
        try
          if DistribuicaoDFePorChaveCTe(AcUFAutor, ACNPJCPF, AchCTe) then
          begin
            Resposta := RespostaDistribuicaoDFe;

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResCTe(i);

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
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'DistribuicaoDFePorChaveCTe');
        except
          raise EACBrLibException.Create(ErrRetorno, WebServices.DistribuicaoDFe.retDistDFeInt.xMotivo);
        end;
      end;

      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_EnviarEmail(const ePara, eChaveCTe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  APara, AChaveCTe, AAssunto, ACC, AAnexos, AMensagem: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
begin
  try
    VerificarLibInicializada;

    APara := string(ePara);
    AChaveCTe := string(eChaveCTe);
    AAssunto := string(eAssunto);
    ACC := string(eCC);
    AAnexos := string(eAnexos);
    AMensagem := string(eMensagem);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CTe_EnviarEmail(' + APara + ',' + AChaveCTe + ',' +
         BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' + ACC + ',' +
         AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_EnviarEmail', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;

      with CTeDM.ACBrCTe1 do
      begin
        EhArquivo := StringEhArquivo(AChaveCTe);

        if EhArquivo then
          VerificarArquivoExiste(AChaveCTe);

        if EhArquivo then
          Conhecimentos.LoadFromFile(AchaveCTe);

        if Conhecimentos.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfCTeCarregados, [Conhecimentos.Count]))
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
                CTeDM.ACBrCTe1.Conhecimentos.Items[0].EnviarEmail(
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

      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_EnviarEmailEvento(const ePara, eChaveEvento, eChaveCTe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  APara, AChaveEvento, AChaveCTe, AAssunto, ACC, AAnexos, AMensagem,
  ArqPDF: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
begin
  try
    VerificarLibInicializada;

    APara := string(ePara);
    AChaveEvento := string(eChaveEvento);
    AChaveCTe := string(eChaveCTe);
    AAssunto := string(eAssunto);
    ACC := string(eCC);
    AAnexos := string(eAnexos);
    AMensagem := string(eMensagem);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CTe_EnviarEmailEvento(' + APara + ',' + AChaveEvento + ',' +
         AChaveCTe + ',' + BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' +
         ACC + ',' + AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_EnviarEmailEvento', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;

      with CTeDM.ACBrCTe1 do
      begin
        EventoCTe.Evento.Clear;
        Conhecimentos.Clear;

        EhArquivo := StringEhArquivo(AChaveEvento);

        if EhArquivo then
          VerificarArquivoExiste(AChaveEvento);

        if EhArquivo then
          EventoCTe.LerXML(AChaveEvento);

        EhArquivo := StringEhArquivo(AChaveCTe);

        if EhArquivo then
          VerificarArquivoExiste(AChaveCTe);

        if EhArquivo then
          Conhecimentos.LoadFromFile(AchaveCTe);

        if EventoCTe.Evento.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio,
                  Format(SInfEventosCarregados, [EventoCTe.Evento.Count]))
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

                ArqPDF := OnlyNumber(EventoCTe.Evento[0].Infevento.id);
                ArqPDF := PathWithDelim(DACTe.PathPDF)+ArqPDF+'-procEventoCTe.pdf';
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
                CTeDM.ACBrCTe1.EnviarEmail(
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

      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_Imprimir: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('CTe_Imprimir', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;
      try
        CTeDM.ACBrCTe1.Conhecimentos.Imprimir;
        Result := SetRetornoCTesCarregados(CTeDM.ACBrCTe1.Conhecimentos.Count);
      finally
        CTeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('CTe_ImprimirPDF', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;
      try
        CTeDM.ACBrCTe1.Conhecimentos.ImprimirPDF;
        Result := SetRetornoCTesCarregados(CTeDM.ACBrCTe1.Conhecimentos.Count);
      finally
        CTeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_ImprimirEvento(const eChaveCTe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AChaveCTe: string;
  AChaveEvento: string;
begin
  try
    VerificarLibInicializada;

    AChaveCTe := string(eChaveCTe);
    AChaveEvento := string(eChaveEvento);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CTe_ImprimirEvento(' + AChaveCTe + ',' + AChaveEvento + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_ImprimirEvento', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;

      EhArquivo := StringEhArquivo(AChaveCTe);

      if EhArquivo then
        VerificarArquivoExiste(AChaveCTe);

      if EhArquivo then
        CTeDM.ACBrCTe1.Conhecimentos.LoadFromFile(AchaveCTe);

      EhArquivo := StringEhArquivo(AChaveEvento);

      if EhArquivo then
        VerificarArquivoExiste(AChaveEvento);

      if EhArquivo then
        CTeDM.ACBrCTe1.EventoCTe.LerXML(AChaveEvento);

      CTeDM.ACBrCTe1.ImprimirEvento;

      Result := SetRetorno(ErrOK);
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_ImprimirEventoPDF(const eChaveCTe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AChaveCTe: string;
  AChaveEvento: string;
begin
  try
    VerificarLibInicializada;

    AChaveCTe := string(eChaveCTe);
    AChaveEvento := string(eChaveEvento);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CTe_ImprimirEventoPDF(' + AChaveCTe + ',' + AChaveEvento + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_ImprimirEventoPDF', logNormal);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;

      EhArquivo := StringEhArquivo(AChaveCTe);

      if EhArquivo then
        VerificarArquivoExiste(AChaveCTe);

      if EhArquivo then
        CTeDM.ACBrCTe1.Conhecimentos.LoadFromFile(AchaveCTe);

      EhArquivo := StringEhArquivo(AChaveEvento);

      if EhArquivo then
        VerificarArquivoExiste(AChaveEvento);

      if EhArquivo then
        CTeDM.ACBrCTe1.EventoCTe.LerXML(AChaveEvento);

      CTeDM.ACBrCTe1.ImprimirEventoPDF;

      Result := SetRetorno(ErrOK);
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_ImprimirInutilizacao(const eChave: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AChave: string;
begin
  try
    VerificarLibInicializada;

    AChave := string(eChave);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CTe_ImprimirInutilizacao(' + AChave + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_ImprimirInutilizacao', logNormal);

    EhArquivo := StringEhArquivo(AChave);

    if EhArquivo then
      VerificarArquivoExiste(AChave);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;

      if EhArquivo then
        CTeDM.ACBrCTe1.InutCTe.LerXML(AChave);

      CTeDM.ACBrCTe1.ImprimirInutilizacao;

      Result := SetRetorno(ErrOK);
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CTe_ImprimirInutilizacaoPDF(const eChave: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AChave: string;
begin
  try
    VerificarLibInicializada;

    AChave := string(eChave);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CTe_ImprimirInutilizacaoPDF(' + AChave + ' )', logCompleto, True)
    else
      pLib.GravarLog('CTe_ImprimirInutilizacaoPDF', logNormal);

    EhArquivo := StringEhArquivo(AChave);

    if EhArquivo then
      VerificarArquivoExiste(AChave);

    with TACBrLibCTe(pLib) do
    begin
      CTeDM.Travar;

      if EhArquivo then
        CTeDM.ACBrCTe1.InutCTe.LerXML(AChave);

      CTeDM.ACBrCTe1.ImprimirInutilizacaoPDF;

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
  pLibClass := TACBrLibCTe;

end.
