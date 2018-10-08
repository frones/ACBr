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

unit ACBrLibMDFeClass;

interface

uses
  Classes, SysUtils, Forms, ACBrLibMailImport,
  ACBrLibComum, ACBrLibMDFeDataModule;

type

  { TACBrLibMDFe }

  TACBrLibMDFe = class(TACBrLib)
  private
    FMDFeDM: TLibMDFeDM;
    FLibMail: TACBrLibMail;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property MDFeDM: TLibMDFeDM read FMDFeDM;
    property LibMail: TACBrLibMail read FLibMail;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function MDFe_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region MDFe}
function MDFe_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_Assinar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_Validar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Servicos}
function RespostaCancelamento: String;
function RespostaDistribuicaoDFe: String;
function RespostaItensDistribuicaoDFeResMDFe(ItemID: integer = 0): String;
function RespostaItensDistribuicaoDFeResEve(ItemID: integer = 0): String;
function RespostaItensDistribuicaoDFeProEve(ItemID: integer = 0): String;
function RespostaItensDistribuicaoDFeInfeve(ItemID: integer = 0): String;
function RespostaEnvio: String;
function RespostaRetorno: String;
function RespostaEvento: String;
function RespostaItensEvento(ItemID: integer = 0): String;

function MDFe_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_Consultar(const eChaveOuMDFe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_Enviar(ALote: Integer; Imprimir: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_EnviarEvento(idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_DistribuicaoDFePorUltNSU(eCNPJCPF, eultNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_DistribuicaoDFePorNSU(eCNPJCPF, eNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_DistribuicaoDFePorChave(eCNPJCPF, echMDFe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function MDFe_EnviarEmail(const ePara, eChaveMDFe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function MDFe_EnviarEmailEvento(const ePara, eChaveEvento, eChaveMDFe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function MDFe_Imprimir: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_ImprimirEvento(const eChaveMDFe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFe_ImprimirEventoPDF(const eChaveMDFe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibMDFeConsts, ACBrLibConfig, ACBrLibResposta,
  ACBrLibMDFeConfig, ACBrLibMDFeRespostas, ACBrMDFe, ACBrMail,
  pcnConversao, pcnAuxiliar, pMDFeConversaoMDFe, blcksock, ACBrUtil;

{ TACBrLibMDFe }

constructor TACBrLibMDFe.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibMDFeNome;
  fpVersao := CLibMDFeVersao;

  FMDFeDM := TLibMDFeDM.Create(nil);
end;

destructor TACBrLibMDFe.Destroy;
begin
  FMDFeDM.Free;
  if FLibMail <> nil then
    FLibMail.Free;

  inherited Destroy;
end;

procedure TACBrLibMDFe.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibMDFe.Inicializar - Inicializando Mail', logParanoico);

  if FileExists(CACBrMailLIBName) then
  begin
    FLibMail := TACBrLibMail.Create(pLib.Config.NomeArquivo, pLib.Config.ChaveCrypt);
    FMDFeDM.ACBrMail1 := FLibMail.GetMail;
  end
  else
  begin
    FMDFeDM.ACBrMail1 := TACBrMail.Create(FMDFeDM);
  end;

  FMDFeDM.ACBrMDFe1.MAIL := FMDFeDM.ACBrMail1;

  GravarLog('TACBrLibMDFe.Inicializar - Inicializando Mail Feito', logParanoico);

  GravarLog('TACBrLibMDFe.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibMDFe.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibMDFeConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibMDFe.Executar;
begin
  inherited Executar;
  FMDFeDM.AplicarConfiguracoes;
end;

{%region MDFe}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function MDFe_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function MDFe_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function MDFe_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function MDFe_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function MDFe_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function MDFe_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function MDFe_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function MDFe_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function MDFe_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;

{%endregion}

function MDFe_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuXml := string(eArquivoOuXML);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_CarregarXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_CarregarXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        if EhArquivo then
          MDFeDM.ACBrMDFe1.Manifestos.LoadFromFile(ArquivoOuXml)
        else
          MDFeDM.ACBrMDFe1.Manifestos.LoadFromString(ArquivoOuXml);

        Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ArquivoOuINI: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuINI := string(eArquivoOuINI);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_CarregarINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_CarregarINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        MDFeDM.ACBrMDFe1.Manifestos.LoadFromIni(ArquivoOuINI);
        Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFe_LimparLista', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        MDFeDM.ACBrMDFe1.Manifestos.Clear;
        Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_Assinar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFe_Assinar', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        try
          MDFeDM.ACBrMDFe1.Manifestos.Assinar;
        except
          on E: EACBrMDFeException do
            Result := SetRetorno(ErrAssinarMDFe, E.Message);
        end;
        Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_Validar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFe_Validar', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        try
          MDFeDM.ACBrMDFe1.Manifestos.Validar;
        except
          on E: EACBrMDFeException do
            Result := SetRetorno(ErrValidacaoMDFe, E.Message);
        end;
        Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Erros: string;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFe_ValidarRegrasdeNegocios', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        Erros := '';
        MDFeDM.ACBrMDFe1.Manifestos.ValidarRegrasdeNegocios(Erros);
        MoverStringParaPChar(Erros, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Erros: string;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFe_VerificarAssinatura', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        Erros := '';
        MDFeDM.ACBrMDFe1.Manifestos.VerificarAssinatura(Erros);
        MoverStringParaPChar(Erros, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        MDFeDM.Destravar;
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
    with TACBrLibMDFe(pLib).MDFeDM.ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfevento do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cOrgao;
      Resp.ChMDFe := chMDFe;
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
    with TACBrLibMDFe(pLib).MDFeDM.ACBrMDFe1.WebServices.DistribuicaoDFe do
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

function RespostaItensDistribuicaoDFeResMDFe(ItemID: integer = 0): String;
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  Resp := TDistribuicaoDFeItemResposta.Create(
    'ResMDFe' + Trim(IntToStrZero(ItemID +1, 3)), resINI);
  try
    with TACBrLibMDFe(pLib).MDFeDM.ACBrMDFe1.WebServices.DistribuicaoDFe do
    begin
      Resp.NSU := retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.XML := retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := listaArqs[ItemID];
      Resp.schema := SchemaMDFeToStr(retDistDFeInt.docZip[ItemID].schema);

      with retDistDFeInt.docZip.Items[ItemID].resMDFe do
      begin
        Resp.chMDFe := chMDFe;
        Resp.CNPJCPF := CNPJCPF;
        Resp.xNome := xNome;
        Resp.IE := IE;
        Resp.dhEmi := dhEmi;
        Resp.vNF := vNF;
        Resp.digVal := digVal;
        Resp.dhRecbto := dhRecbto;
        Resp.cSitMDFe := SituacaoDFeToStr(cSitMDFe);
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
    with fACBrMDFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].resEvento do
    begin
      Resp.NSU := fACBrMDFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.chMDFe := chMDFe;
      Resp.CNPJCPF := CNPJCPF;
      Resp.dhEvento := dhEvento;
      Resp.tpEvento := TpEventoToStr(tpEvento);
      Resp.xEvento := xEvento;
      Resp.nSeqEvento := nSeqEvento;
      Resp.cOrgao := cOrgao;
      Resp.dhRecbto := dhRecbto;
      Resp.nProt := nProt;
      Resp.XML := fACBrMDFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := fACBrMDFe.WebServices.DistribuicaoDFe.listaArqs[ItemID];
      Resp.schema := SchemaMDFeToStr(fACBrMDFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[ItemID].schema);

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
    with TACBrLibMDFe(pLib).MDFeDM.ACBrMDFe1.WebServices.DistribuicaoDFe do
    begin
      Resp.NSU := retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.XML := retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := listaArqs[ItemID];
      Resp.schema := SchemaMDFeToStr(retDistDFeInt.docZip[ItemID].schema);

      with retDistDFeInt.docZip.Items[ItemID].procEvento do
      begin
        Resp.chMDFe := chMDFe;
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
          Resp.cteNProt := cte.nProt;
          Resp.cteChvCTe := cte.chCTe;
          Resp.cteDhemi := cte.dhEmi;
          Resp.cteModal := TpModalToStr(cte.modal);
          Resp.cteDhRebcto := cte.dhRecbto;
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
    with TACBrLibMDFe(pLib).MDFeDM.ACBrMDFe1.WebServices.DistribuicaoDFe do
    begin
      Resp.XML := retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := listaArqs[ItemID];
      Resp.schema := SchemaMDFeToStr(retDistDFeInt.docZip[ItemID].schema);

      with retDistDFeInt.docZip.Items[ItemID].procEvento.RetInfevento do
      begin
        Resp.Id := Id;
        Resp.VerAplic := VerAplic;
        Resp.tpAmb := TpAmbToStr(tpAmb);
        Resp.cOrgao := cOrgao;
        Resp.chMDFe := chMDFe;
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
    with TACBrLibMDFe(pLib).MDFeDM.ACBrMDFe1.WebServices.Enviar do
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
    with TACBrLibMDFe(pLib).MDFeDM.ACBrMDFe1.WebServices.Retorno do
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
    with TACBrLibMDFe(pLib).MDFeDM.ACBrMDFe1.WebServices.EnvEvento.EventoRetorno do
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
    with TACBrLibMDFe(pLib).MDFeDM.ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[ItemID].RetInfevento do
    begin
      Resp.Id := Id;
      Resp.tpAmb := TpAmbToStr(tpAmb);
      Resp.verAplic := verAplic;
      Resp.cOrgao := cOrgao;
      Resp.cStat := cStat;
      Resp.xMotivo := xMotivo;
      Resp.chMDFe := chMDFe;
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

function MDFe_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: TStatusServicoResposta;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFe_StatusServico', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      Resposta := TStatusServicoResposta.Create(pLib.Config.TipoResposta);
      try
        with MDFeDM.ACBrMDFe1 do
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
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_Consultar(const eChaveOuMDFe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ChaveOuMDFe: string;
  Resposta: TConsultaResposta;
begin
  try
    VerificarLibInicializada;

    ChaveOuMDFe := string(eChaveOuMDFe);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_Consultar(' + ChaveOuMDFe + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_Consultar', logNormal);

    EhArquivo := StringEhArquivo(ChaveOuMDFe);
    if EhArquivo then
      VerificarArquivoExiste(ChaveOuMDFe);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      if EhArquivo then
        MDFeDM.ACBrMDFe1.Manifestos.LoadFromFile(ChaveOuMDFe);

      if MDFeDM.ACBrMDFe1.Manifestos.Count = 0 then
      begin
        if ValidarChave(ChaveOuMDFe) then
          MDFeDM.ACBrMDFe1.WebServices.Consulta.MDFeChave := ChaveOuMDFe
        else
          raise EACBrLibException.Create(ErrChaveMDFe, Format(SErrChaveInvalida, [ChaveOuMDFe]));
      end
      else
        MDFeDM.ACBrMDFe1.WebServices.Consulta.MDFeChave := StringReplace(
          MDFeDM.ACBrMDFe1.Manifestos.Items[MDFeDM.ACBrMDFe1.Manifestos.Count - 1].MDFe.infMDFe.ID,
          'MDFe','',[rfIgnoreCase]);

      Resposta := TConsultaResposta.Create(pLib.Config.TipoResposta);
      try
        with MDFeDM.ACBrMDFe1 do
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
            Resposta.ChMDFe := WebServices.Consulta.MDFeChave;
            Resposta.NProt := WebServices.Consulta.Protocolo;
            Resposta.DigVal := WebServices.Consulta.protMDFe.digVal;

            MoverStringParaPChar(Resposta.Gerar, sResposta, esTamanho);
            Result := SetRetorno(ErrOK, StrPas(sResposta));
          end
          else
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'Consultar');
        end;
      finally
        Resposta.Free;
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_Enviar(ALote: Integer; Imprimir: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: String;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_Enviar(' + IntToStr(ALote) + ',' +
                   BoolToStr(Imprimir, 'Imprimir','') + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_Enviar', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      with MDFeDM.ACBrMDFe1 do
      begin
        if Manifestos.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfMDFeCarregados, [Manifestos.Count]))
        else
        begin
          Manifestos.Assinar;
          Manifestos.Validar;

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

      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
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
      pLib.GravarLog('MDFe_Cancelar(' + AChave + ',' + AJustificativa + ',' +
                        ACNPJ + ',' + IntToStr(ALote) + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_Cancelar', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      if not ValidarChave(AChave) then
        raise EACBrLibException.Create(ErrChaveMDFe, Format(SErrChaveInvalida, [AChave]))
      else
        MDFeDM.ACBrMDFe1.WebServices.Consulta.MDFeChave := AChave;

      if not MDFeDM.ACBrMDFe1.WebServices.Consulta.Executar then
        raise EACBrLibException.Create(ErrConsulta, MDFeDM.ACBrMDFe1.WebServices.Consulta.Msg);

      MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Clear;

      with MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Add do
      begin
        Infevento.CNPJCPF := ACNPJ;
        if Trim(Infevento.CNPJCPF) = '' then
          Infevento.CNPJCPF := copy(OnlyNumber(MDFeDM.ACBrMDFe1.WebServices.Consulta.MDFeChave), 7, 14)
        else
        begin
          if not ValidarCNPJ(ACNPJ) then
            raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJInvalido, [ACNPJ]));
        end;

        Infevento.cOrgao := StrToIntDef(
          copy(OnlyNumber(MDFeDM.ACBrMDFe1.WebServices.Consulta.MDFeChave), 1, 2), 0);
        Infevento.dhEvento := now;
        Infevento.tpEvento := teCancelamento;
        Infevento.chMDFe := MDFeDM.ACBrMDFe1.WebServices.Consulta.MDFeChave;
        Infevento.detEvento.nProt := MDFeDM.ACBrMDFe1.WebServices.Consulta.Protocolo;
        Infevento.detEvento.xJust := AJustificativa;
      end;

      try
        if MDFeDM.ACBrMDFe1.EnviarEvento(ALote) then
        begin
          Resposta := RespostaCancelamento;

          MoverStringParaPChar(Resposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, StrPas(sResposta));
        end
        else
          Result := SetRetornoWebService(MDFeDM.ACBrMDFe1.SSL.HTTPResultCode, 'Cancelar');
      except
        raise EACBrLibException.Create(ErrRetorno, MDFeDM.ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.xMotivo);
      end;

      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_EnviarEvento(idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: String;
  I: Integer;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_EnviarEvento(' + IntToStr(idLote) + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_EnviarEvento', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      with MDFeDM.ACBrMDFe1 do
      begin
        if EventoMDFe.Evento.Count =0 then
          raise EACBrLibException.Create(ErrEnvioEvento, Format(SInfEventosCarregados, [EventoMDFe.Evento.Count]))
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

      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_DistribuicaoDFePorUltNSU(eCNPJCPF, eultNSU: PChar;
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
      pLib.GravarLog('MDFe_DistribuicaoDFePorUltNSU(' +
                     ACNPJCPF + ',' + AultNSU + ',' + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_DistribuicaoDFePorUltNSU', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      if not ValidarCNPJ(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with MDFeDM.ACBrMDFe1 do
      begin
        try
          if DistribuicaoDFePorUltNSU(ACNPJCPF, AultNSU) then
          begin
            Resposta := RespostaDistribuicaoDFe;

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResMDFe(i);

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

      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_DistribuicaoDFePorNSU(eCNPJCPF, eNSU: PChar;
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
      pLib.GravarLog('MDFe_DistribuicaoDFePorNSU(' +
                     ACNPJCPF + ',' + ANSU + ',' + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_DistribuicaoDFePorNSU', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      if not ValidarCNPJ(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with MDFeDM.ACBrMDFe1 do
      begin
        try
          if DistribuicaoDFePorNSU(ACNPJCPF, ANSU) then
          begin
            Resposta := RespostaDistribuicaoDFe;

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResMDFe(i);

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

      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_DistribuicaoDFePorChave(eCNPJCPF, echMDFe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AchMDFe, ACNPJCPF: string;
  Resposta: string;
  i: Integer;
begin
  try
    VerificarLibInicializada;

    ACNPJCPF := string(eCNPJCPF);
    AchMDFe := string(echMDFe);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_DistribuicaoDFePorChave(' +
                     ACNPJCPF + ',' + AchMDFe + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_DistribuicaoDFePorChave', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      if not ValidarCNPJ(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      if not ValidarChave(AchMDFe) then
        raise EACBrLibException.Create(ErrChaveMDFe, Format(SErrChaveInvalida, [AchMDFe]));

      with MDFeDM.ACBrMDFe1 do
      begin
        try
          {
          if DistribuicaoDFePorChaveMDFe(ACNPJCPF, AchMDFe) then
          begin
            Resposta := RespostaDistribuicaoDFe;

            for i := 0 to WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
              Resposta := Resposta + RespostaItensDistribuicaoDFeResMDFe(i);

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
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'DistribuicaoDFePorChaveMDFe');
          }
        except
          raise EACBrLibException.Create(ErrRetorno, WebServices.DistribuicaoDFe.retDistDFeInt.xMotivo);
        end;
      end;

      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_EnviarEmail(const ePara, eChaveMDFe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  APara, AChaveMDFe, AAssunto, ACC, AAnexos, AMensagem: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
begin
  try
    VerificarLibInicializada;

    APara := string(ePara);
    AChaveMDFe := string(eChaveMDFe);
    AAssunto := string(eAssunto);
    ACC := string(eCC);
    AAnexos := string(eAnexos);
    AMensagem := string(eMensagem);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_EnviarEmail(' + APara + ',' + AChaveMDFe + ',' +
         BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' + ACC + ',' +
         AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_EnviarEmail', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      with MDFeDM.ACBrMDFe1 do
      begin
        EhArquivo := StringEhArquivo(AChaveMDFe);

        if EhArquivo then
          VerificarArquivoExiste(AChaveMDFe);

        if EhArquivo then
          Manifestos.LoadFromFile(AchaveMDFe);

        if Manifestos.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfMDFeCarregados, [Manifestos.Count]))
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
                MDFeDM.ACBrMDFe1.Manifestos.Items[0].EnviarEmail(
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

      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_EnviarEmailEvento(const ePara, eChaveEvento, eChaveMDFe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  APara, AChaveEvento, AChaveMDFe, AAssunto, ACC, AAnexos, AMensagem,
  ArqPDF: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
begin
  try
    VerificarLibInicializada;

    APara := string(ePara);
    AChaveEvento := string(eChaveEvento);
    AChaveMDFe := string(eChaveMDFe);
    AAssunto := string(eAssunto);
    ACC := string(eCC);
    AAnexos := string(eAnexos);
    AMensagem := string(eMensagem);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_EnviarEmailEvento(' + APara + ',' + AChaveEvento + ',' +
         AChaveMDFe + ',' + BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' +
         ACC + ',' + AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_EnviarEmailEvento', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      with MDFeDM.ACBrMDFe1 do
      begin
        EventoMDFe.Evento.Clear;
        Manifestos.Clear;

        EhArquivo := StringEhArquivo(AChaveEvento);

        if EhArquivo then
          VerificarArquivoExiste(AChaveEvento);

        if EhArquivo then
          EventoMDFe.LerXML(AChaveEvento);

        EhArquivo := StringEhArquivo(AChaveMDFe);

        if EhArquivo then
          VerificarArquivoExiste(AChaveMDFe);

        if EhArquivo then
          Manifestos.LoadFromFile(AchaveMDFe);

        if EventoMDFe.Evento.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio,
                  Format(SInfEventosCarregados, [EventoMDFe.Evento.Count]))
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

                ArqPDF := OnlyNumber(EventoMDFe.Evento[0].Infevento.id);
                ArqPDF := PathWithDelim(DAMDFe.PathPDF)+ArqPDF+'-procEventoMDFe.pdf';
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
                MDFeDM.ACBrMDFe1.EnviarEmail(
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

      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_Imprimir: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFe_Imprimir', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        MDFeDM.ACBrMDFe1.Manifestos.Imprimir;
        Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFe_ImprimirPDF', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        MDFeDM.ACBrMDFe1.Manifestos.ImprimirPDF;
        Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_ImprimirEvento(const eChaveMDFe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AChaveMDFe: string;
  AChaveEvento: string;
begin
  try
    VerificarLibInicializada;

    AChaveMDFe := string(eChaveMDFe);
    AChaveEvento := string(eChaveEvento);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_ImprimirEvento(' + AChaveMDFe + ',' + AChaveEvento + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_ImprimirEvento', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      EhArquivo := StringEhArquivo(AChaveMDFe);

      if EhArquivo then
        VerificarArquivoExiste(AChaveMDFe);

      if EhArquivo then
        MDFeDM.ACBrMDFe1.Manifestos.LoadFromFile(AchaveMDFe);

      EhArquivo := StringEhArquivo(AChaveEvento);

      if EhArquivo then
        VerificarArquivoExiste(AChaveEvento);

      if EhArquivo then
        MDFeDM.ACBrMDFe1.EventoMDFe.LerXML(AChaveEvento);

      MDFeDM.ACBrMDFe1.ImprimirEvento;

      Result := SetRetorno(ErrOK);
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFe_ImprimirEventoPDF(const eChaveMDFe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AChaveMDFe: string;
  AChaveEvento: string;
begin
  try
    VerificarLibInicializada;

    AChaveMDFe := string(eChaveMDFe);
    AChaveEvento := string(eChaveEvento);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_ImprimirEventoPDF(' + AChaveMDFe + ',' + AChaveEvento + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_ImprimirEventoPDF', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      EhArquivo := StringEhArquivo(AChaveMDFe);

      if EhArquivo then
        VerificarArquivoExiste(AChaveMDFe);

      if EhArquivo then
        MDFeDM.ACBrMDFe1.Manifestos.LoadFromFile(AchaveMDFe);

      EhArquivo := StringEhArquivo(AChaveEvento);

      if EhArquivo then
        VerificarArquivoExiste(AChaveEvento);

      if EhArquivo then
        MDFeDM.ACBrMDFe1.EventoMDFe.LerXML(AChaveEvento);

      MDFeDM.ACBrMDFe1.ImprimirEventoPDF;

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
  pLibClass := TACBrLibMDFe;

end.
