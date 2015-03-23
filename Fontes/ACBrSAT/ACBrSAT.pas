{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: André Ferreira de Moraes                        }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrSAT;

interface

uses
  Classes, SysUtils, pcnCFe, pcnRede, pcnCFeCanc, ACBrBase, ACBrSATClass,
  ACBrSATExtratoClass, synacode, StrUtils
  {$IFNDEF NOGUI}
    {$IFDEF FPC} ,LResources {$ENDIF}
  {$ENDIF};

const CPREFIXO_CFe = 'CFe';

type
   { TACBrSAT }

   TACBrSAT = class( TACBrComponent )
   private
     fsCFe : TCFe ;
     fsCFeCanc : TCFeCanc ;
     fsnumeroSessao : Integer ;
     fsOnGetcodigoDeAtivacao : TACBrSATGetChave ;
     fsOnGetsignAC : TACBrSATGetChave ;
     fsOnGravarLog : TACBrGravarLog ;
     fsNomeDLL : String ;
     fsPrefixoCFe: String;
     fsResposta : TACBrSATResposta ;
     fsRespostaComando : String ;
     fsSATClass : TACBrSATClass ;
     fsExtrato : TACBrSATExtratoClass;

     fsArqLOG: String;
     fsComandoLog: String;
     fsInicializado : Boolean ;
     fsModelo : TACBrSATModelo ;
     fsConfig : TACBrSATConfig ;
     fsRede   : TRede ;
     fsStatus : TACBrSATStatus;

     fsSalvarCFes: Boolean;
     fsPastaCFeCancelamento: String;
     fsPastaCFeVenda: String;

     function CodificarPaginaDeCodigoSAT(ATexto: String): AnsiString;
     function DecodificarPaginaDeCodigoSAT(ATexto: AnsiString): String;

     function GetAbout : String;
     function GetcodigoDeAtivacao : AnsiString ;
     function GetModeloStrClass : String ;
     function GetsignAC : AnsiString ;
     procedure SetAbout(const Value: String);{%h-}
     procedure SetInicializado(AValue : Boolean) ;
     procedure SetModelo(AValue : TACBrSATModelo) ;
     procedure SetNomeDLL(AValue : string) ;

     function GetPastaCFeCancelamento: String;
     function GetPastaCFeVenda: String;
     procedure SetPastaCFeCancelamento(AValue: String);
     procedure SetPastaCFeVenda(AValue: String);

     procedure IniciaComando ;
     function FinalizaComando(AResult: String): String;
     procedure VerificaCondicoesImpressao( EhCancelamento: Boolean = False);

     procedure GravaLog(AString : AnsiString ) ;
     procedure SetExtrato(const Value: TACBrSATExtratoClass);
   protected
     procedure Notification(AComponent: TComponent; Operation: TOperation); override;
   public
     property SAT : TACBrSATClass read fsSATClass ;

     constructor Create( AOwner : TComponent ) ; override;
     destructor Destroy ; override;

     Procedure Inicializar;
     Procedure DesInicializar;
     property Inicializado : Boolean read fsInicializado write SetInicializado ;
     procedure VerificaInicializado ;

     Property ModeloStr : String  read GetModeloStrClass;

     property numeroSessao : Integer read fsnumeroSessao write fsnumeroSessao;
     function GerarnumeroSessao : Integer ;

     property codigoDeAtivacao : AnsiString read GetcodigoDeAtivacao ;
     property signAC           : AnsiString read GetsignAC ;

     property RespostaComando: String read fsRespostaComando ;

     property CFe : TCFe read fsCFe ;
     property CFeCanc : TCFeCanc read fsCFeCanc ;

     property Status : TACBrSATStatus read fsStatus;
     property Resposta : TACBrSATResposta read fsResposta;
     property PrefixoCFe: String read fsPrefixoCFe;

     procedure InicializaCFe( ACFe : TCFe = nil );

     procedure DoLog(AString : String ) ;

     function AssociarAssinatura( CNPJvalue, assinaturaCNPJs : AnsiString ): String ;
     function AtivarSAT(subComando : Integer ; CNPJ : AnsiString ; cUF : Integer
       ) : String ;
     function AtualizarSoftwareSAT : String ;
     function BloquearSAT : String ;
     procedure CFe2CFeCanc;
     function CancelarUltimaVenda :String ; overload;
     function CancelarUltimaVenda( chave, dadosCancelamento : AnsiString ) :
       String ; overload;
     function ComunicarCertificadoICPBRASIL( certificado : AnsiString ) :
       String ;
     function ConfigurarInterfaceDeRede( dadosConfiguracao : AnsiString = '') :
       String ;
     function ConsultarNumeroSessao( cNumeroDeSessao : Integer) : String ;
     function ConsultarSAT : String ;
     function ConsultarStatusOperacional : String ;
     function DesbloquearSAT : String ;
     function EnviarDadosVenda : String ; overload;
     function EnviarDadosVenda( dadosVenda : AnsiString ) : String ; overload;
     procedure ExtrairLogs( NomeArquivo : String ); overload;
     procedure ExtrairLogs( AStringList : TStrings ); overload;
     procedure ExtrairLogs( AStream : TStream ); overload;
     function TesteFimAFim( dadosVenda : AnsiString) : String ;
     function TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia: AnsiString;
       opcao: Integer; novoCodigo: AnsiString): String;

    procedure ImprimirExtrato;
    procedure ImprimirExtratoResumido;
    procedure ImprimirExtratoCancelamento;

   published
     property Modelo : TACBrSATModelo read fsModelo write SetModelo
                 default satNenhum ;

     property Extrato: TACBrSATExtratoClass read fsExtrato write SetExtrato ;

     property NomeDLL: string read fsNomeDLL write SetNomeDLL;

     property About : String read GetAbout write SetAbout stored False ;
     property ArqLOG : String read fsArqLOG write fsArqLOG ;
     property OnGravarLog : TACBrGravarLog read fsOnGravarLog write fsOnGravarLog;

     property Config : TACBrSATConfig read fsConfig write fsConfig;
     property Rede : TRede read fsRede write fsRede;

     property OnGetcodigoDeAtivacao : TACBrSATGetChave read fsOnGetcodigoDeAtivacao
        write fsOnGetcodigoDeAtivacao;
     property OnGetsignAC : TACBrSATGetChave read fsOnGetsignAC write fsOnGetsignAC;

     property SalvarCFes: Boolean read fsSalvarCFes write fsSalvarCFes default false;
     property PastaCFeVenda: String read GetPastaCFeVenda write SetPastaCFeVenda;
     property PastaCFeCancelamento: String read GetPastaCFeCancelamento
        write SetPastaCFeCancelamento;
   end;


implementation

Uses ACBrUtil, ACBrSATDinamico_cdecl, ACBrSATDinamico_stdcall;

{ TACBrSAT }

constructor TACBrSAT.Create(AOwner : TComponent) ;
begin
  inherited Create(AOwner) ;

  fsnumeroSessao    := 0;
  fsNomeDLL         := '';
  fsArqLOG          := '' ;
  fsComandoLog      := '';
  fsRespostaComando := '';

  fsOnGetcodigoDeAtivacao := nil;
  fsOnGetsignAC           := nil;
  fsOnGravarLog           := nil;

  fsConfig  := TACBrSATConfig.Create;
  fsRede    := TRede.Create;
  fsCFe     := TCFe.Create;
  fsCFeCanc := TCFeCanc.Create;
  fsResposta:= TACBrSATResposta.Create;
  fsStatus  := TACBrSATStatus.Create;
  fsSATClass:= TACBrSATClass.Create( Self ) ;

  fsSalvarCFes           := False;
  fsPastaCFeCancelamento := '';
  fsPastaCFeVenda        := '';
  fsPrefixoCFe           := CPREFIXO_CFe;
end ;

destructor TACBrSAT.Destroy ;
begin
  fsConfig.Free;
  fsRede.Free;
  fsCFe.Free;
  fsCFeCanc.Free;
  fsResposta.Free;
  fsStatus.Free;

  if Assigned( fsSATClass ) then
    FreeAndNil( fsSATClass );

  inherited Destroy ;
end ;

procedure TACBrSAT.Inicializar ;
begin
  if fsInicializado then exit ;

  if fsModelo = satNenhum then
     raise EACBrSATErro.Create( cACBrSATModeloNaoDefinido );

  fsSATClass.Inicializar ;
  Randomize;

  DoLog( 'ACBrSAT.Inicializado');
  fsInicializado := true ;
  fsPrefixoCFe := CPREFIXO_CFe;
end ;

procedure TACBrSAT.DesInicializar ;
begin
  if not fsInicializado then exit ;

  fsSATClass.DesInicializar ;

  DoLog( 'ACBrSAT.DesInicializado');
  fsInicializado := false;
end ;

procedure TACBrSAT.VerificaInicializado ;
begin
  if not Inicializado then
     raise EACBrSATErro.Create( cACBrSATNaoInicializado ) ;
end ;

procedure TACBrSAT.IniciaComando ;
var
  AStr : String ;
begin
  VerificaInicializado;
  GerarnumeroSessao;

  fsRespostaComando := '';
  AStr := '-- '+FormatDateTime('hh:nn:ss:zzz',now) +
          ' - numeroSessao: '+IntToStr(numeroSessao) ;
  if fsComandoLog <> '' then
     AStr := AStr + ' - Comando: '+fsComandoLog;

  DoLog( AStr );
end ;

function TACBrSAT.FinalizaComando( AResult : String ) : String ;
var
  AStr : String ;
begin
  fsRespostaComando := DecodificarPaginaDeCodigoSAT( AResult );
  Result := fsRespostaComando;

  fsComandoLog := '';
  AStr := 'NumeroSessao: '+IntToStr(numeroSessao) ;
  if fsRespostaComando <> '' then
     AStr := AStr + ' - Resposta:'+fsRespostaComando;

  Resposta.RetornoStr := fsRespostaComando;

  DoLog( AStr );
end ;

procedure TACBrSAT.VerificaCondicoesImpressao(EhCancelamento: Boolean);
begin
  if not Assigned(Extrato) then
    raise EACBrSATErro.Create( 'Nenhum componente "ACBrSATExtrato" associado' ) ;

  if EhCancelamento then
  begin
    if (CFeCanc.infCFe.ID = '') and (CFe.infCFe.ID = '') then
      raise EACBrSATErro.Create( 'Nenhum CFeCanc ou CFe carregado na memória' ) ;
  end
  else
  begin
    if (CFe.infCFe.ID = '') then
      raise EACBrSATErro.Create( 'Nenhum CFe carregado na memória' ) ;
  end;
end;

procedure TACBrSAT.DoLog(AString : String) ;
var
  Tratado: Boolean;
begin
  Tratado := False;
  if Assigned( fsOnGravarLog ) then
    fsOnGravarLog( AString, Tratado );

  if not Tratado then
    GravaLog( AString );
end ;

procedure TACBrSAT.GravaLog(AString : AnsiString) ;
begin
  if (ArqLOG = '') then
    exit;

  WriteLog( ArqLOG, ' - '+FormatDateTime('hh:nn:ss:zzz',now) + ' - ' + AString );
end ;

function TACBrSAT.GerarnumeroSessao : Integer ;
begin
  fsnumeroSessao := Random(999999);
  Result := fsnumeroSessao;
end ;

procedure TACBrSAT.InicializaCFe(ACFe : TCFe) ;
Var
  wCFe : TCFe ;
begin
  if Assigned( ACFe ) then
    wCFe := ACFe
  else
    wCFe := fsCFe;

  with wCFe do
  begin
    Clear;
    ide.CNPJ              := fsConfig.ide_CNPJ;
    ide.tpAmb             := fsConfig.ide_tpAmb;
    ide.numeroCaixa       := fsConfig.ide_numeroCaixa;
    ide.signAC            := signAC;
    Emit.CNPJCPF          := fsConfig.emit_CNPJ;
    Emit.IE               := fsConfig.emit_IE;
    Emit.IM               := fsConfig.emit_IM;
    Emit.cRegTrib         := fsConfig.emit_cRegTrib;
    Emit.cRegTribISSQN    := fsConfig.emit_cRegTribISSQN;
    Emit.indRatISSQN      := fsConfig.emit_indRatISSQN;
    infCFe.versaoDadosEnt := fsConfig.infCFe_versaoDadosEnt;
  end ;
end ;

function TACBrSAT.AssociarAssinatura(CNPJvalue, assinaturaCNPJs : AnsiString
  ) : String ;
begin
  fsComandoLog := 'AssociarAssinatura( '+CNPJvalue+', '+assinaturaCNPJs+' )';
  IniciaComando;
  Result := FinalizaComando( fsSATClass.AssociarAssinatura( CNPJvalue, assinaturaCNPJs ) );
end ;

function TACBrSAT.AtivarSAT(subComando : Integer ; CNPJ : AnsiString ;
  cUF : Integer) : String ;
begin
  fsComandoLog := 'AtivarSAT( '+IntToStr(subComando)+', '+CNPJ+', '+IntToStr(cUF)+' )';
  IniciaComando;
  Result := FinalizaComando( fsSATClass.AtivarSAT( subComando, CNPJ, cUF ) );
end ;

function TACBrSAT.AtualizarSoftwareSAT : String ;
begin
  fsComandoLog := 'AtualizarSoftwareSAT';
  IniciaComando;
  Result := FinalizaComando( fsSATClass.AtualizarSoftwareSAT );
end ;

function TACBrSAT.BloquearSAT : String ;
begin
  fsComandoLog := 'BloquearSAT';
  IniciaComando;
  Result := FinalizaComando( fsSATClass.BloquearSAT );
end ;

function TACBrSAT.CancelarUltimaVenda: String ;
var
  dadosCancelamento : string;
begin
  if CFeCanc.infCFe.chCanc = '' then
     CFe2CFeCanc;

  dadosCancelamento := CFeCanc.GetXMLString( true ); // True = Gera apenas as TAGs da aplicação

  Result := CancelarUltimaVenda( CFeCanc.infCFe.chCanc, dadosCancelamento);
end ;


function TACBrSAT.CancelarUltimaVenda(chave, dadosCancelamento : AnsiString
  ) : String ;
var
  XMLRecebido, NomeCFe: String;
begin
  fsComandoLog := 'CancelarUltimaVenda( '+chave+', '+dadosCancelamento+' )';

  if Trim(chave) = '' then
     raise EACBrSATErro.Create('Parâmetro: "chave" não informado');

  if Trim(dadosCancelamento) = '' then
     raise EACBrSATErro.Create('Parâmetro: "dadosCancelamento" não informado');

  if SalvarCFes then
  begin
    ForceDirectories( PastaCFeCancelamento );
    NomeCFe := PastaCFeCancelamento + PathDelim + chave + '-can-env.xml';
    WriteToTXT(NomeCFe, dadosCancelamento, False, False);
  end;

  IniciaComando;
  Result := FinalizaComando( fsSATClass.CancelarUltimaVenda(chave, dadosCancelamento) ) ;

(*
  // Workaround para SAT Kryptus, que usa o prefixo como: "Cfe" ao inves de "CFe"
  if (fsResposta.codigoDeRetorno = 7007) and (LeftStr(chave,3) = CPREFIXO_CFe) then
  begin
    fsPrefixoCFe      := 'Cfe';                      // Ajusta o Prefixo
    ChaveAntiga       := chave;
    chave             := fsPrefixoCFe + copy(chave,4,Length(chave));
    dadosCancelamento := StringReplace( dadosCancelamento, ChaveAntiga, chave, [rfReplaceAll] );
    CancelarUltimaVenda( chave, dadosCancelamento);  // Tenta novamente
    exit;                                            // cai fora por já tratou na chamada acima
  end;
*)

  if fsResposta.codigoDeRetorno = 7000 then
  begin
     XMLRecebido := DecodeBase64(fsResposta.RetornoLst[6]);
     CFeCanc.AsXMLString := XMLRecebido;

     if SalvarCFes then
     begin
       NomeCFe := PastaCFeCancelamento + PathDelim + chave + '-can.xml';
       WriteToTXT(NomeCFe, XMLRecebido, False, False);
     end;
  end;
end ;

function TACBrSAT.ComunicarCertificadoICPBRASIL(certificado : AnsiString
  ) : String ;
begin
  fsComandoLog := 'ComunicarCertificadoICPBRASIL( '+certificado+' )';
  IniciaComando;
  Result := FinalizaComando( fsSATClass.ComunicarCertificadoICPBRASIL( certificado ) );
end ;

function TACBrSAT.ConfigurarInterfaceDeRede(dadosConfiguracao : AnsiString
  ) : String ;
begin
  if dadosConfiguracao = '' then
    dadosConfiguracao := Rede.AsXMLString
  else
    Rede.AsXMLString := dadosConfiguracao;

  fsComandoLog := 'ConfigurarInterfaceDeRede( '+dadosConfiguracao+' )';
  IniciaComando;
  Result := FinalizaComando( fsSATClass.ConfigurarInterfaceDeRede( dadosConfiguracao ) );
end ;

function TACBrSAT.ConsultarNumeroSessao(cNumeroDeSessao : Integer
  ) : String ;
begin
  fsComandoLog := 'ConsultarNumeroSessao( '+IntToStr(cNumeroDeSessao)+' )';
  IniciaComando;
  Result := FinalizaComando( fsSATClass.ConsultarNumeroSessao( cNumeroDeSessao ) );
end ;

function TACBrSAT.ConsultarSAT : String ;
begin
  fsComandoLog := 'ConsultarSAT';
  IniciaComando;
  Result := FinalizaComando( fsSATClass.ConsultarSAT );
end ;

function TACBrSAT.ConsultarStatusOperacional : String ;
Var
  ok: Boolean;
  I: Integer;
begin
  fsComandoLog := 'ConsultarStatusOperacional';
  IniciaComando;
  Result := FinalizaComando( fsSATClass.ConsultarStatusOperacional ) ;
  ok := True;

  if fsResposta.codigoDeRetorno = 10000 then
  begin
    with fsRede do
    begin
      tipoLan := StrToTipoLan(ok, fsResposta.RetornoLst[06]) ;
      lanIP   := fsResposta.RetornoLst[07];
      lanMask := fsResposta.RetornoLst[09];
      lanGW   := fsResposta.RetornoLst[10];
      lanDNS1 := fsResposta.RetornoLst[11];
      lanDNS2 := fsResposta.RetornoLst[12];
    end;

    with fsStatus do
    begin
      Clear;
      NSERIE         := fsResposta.RetornoLst[05];
      LAN_MAC        := fsResposta.RetornoLst[08];
      STATUS_LAN     := StrToStatusLan(ok, fsResposta.RetornoLst[13]) ;;
      NIVEL_BATERIA  := StrToNivelBateria(ok, fsResposta.RetornoLst[14]) ;;
      MT_TOTAL       := fsResposta.RetornoLst[15];
      MT_USADA       := fsResposta.RetornoLst[16];
      DH_ATUAL       := StoD( fsResposta.RetornoLst[17] );
      VER_SB         := fsResposta.RetornoLst[18];
      VER_LAYOUT     := fsResposta.RetornoLst[19];
      ULTIMO_CFe     := fsResposta.RetornoLst[20];
      LISTA_INICIAL  := fsResposta.RetornoLst[21];

      { Workaround para leitura de Status do Emulador do Fiscl,
        que não retorna o campo: LISTA_FINAL }
      I := 22;
      if fsResposta.RetornoLst.Count > 27 then
      begin
        LISTA_FINAL    := fsResposta.RetornoLst[22];
        Inc(I);
      end;
      DH_CFe         := StoD( fsResposta.RetornoLst[I] );
      Inc(I);
      DH_ULTIMA      := StoD( fsResposta.RetornoLst[I] );
      Inc(I);
      CERT_EMISSAO   := StoD( fsResposta.RetornoLst[I] ) ;
      Inc(I);
      CERT_VENCIMENTO:= StoD( fsResposta.RetornoLst[I] ) ;
      Inc(I);
      ESTADO_OPERACAO:= StrToEstadoOperacao(ok, fsResposta.RetornoLst[I]) ;;
    end;
  end;
end ;

function TACBrSAT.DesbloquearSAT : String ;
begin
  fsComandoLog := 'DesbloquearSAT';
  IniciaComando;
  Result := FinalizaComando( fsSATClass.DesbloquearSAT );
end ;

function TACBrSAT.EnviarDadosVenda: String;
begin
  Result := EnviarDadosVenda( CFe.GetXMLString( True ) );  // True = Gera apenas as TAGs da aplicação
end;

function TACBrSAT.EnviarDadosVenda(dadosVenda : AnsiString) : String ;
var
  XMLRecebido, NomeCFe : String;
begin
  fsComandoLog := 'EnviarDadosVenda( '+dadosVenda+' )';
  if Trim(dadosVenda) = '' then
     raise EACBrSATErro.Create('Parâmetro: "dadosVenda" não informado');

  IniciaComando;

  if SalvarCFes then
  begin
    ForceDirectories( PastaCFeVenda );
    NomeCFe := PastaCFeVenda + PathDelim +
               FormatDateTime('YYYYMMDDHHNNSS',Now) + '-' +
               IntToStrZero(numeroSessao, 6) + '-cfe-env.xml';
    WriteToTXT(NomeCFe, dadosVenda, False, False);
  end;

  Result := FinalizaComando( fsSATClass.EnviarDadosVenda( Trim(dadosVenda) ) );

  if fsResposta.codigoDeRetorno = 6000 then
  begin
     XMLRecebido := DecodeBase64(fsResposta.RetornoLst[6]);
     CFe.AsXMLString := XMLRecebido;

     if SalvarCFes then
     begin
       NomeCFe := PastaCFeVenda + PathDelim + CPREFIXO_CFe + CFe.infCFe.ID + '.xml';
       WriteToTXT(NomeCFe, XMLRecebido, False, False);
     end;
  end;
end ;

procedure TACBrSAT.ExtrairLogs(NomeArquivo: String);
var
  SL: TStringList;
begin
  if NomeArquivo = '' then
    raise EACBrSATErro.Create('Nome para Arquivo de Log não especificado');

  SL := TStringList.Create;
  try
    SL.Clear;
    ExtrairLogs(SL);
    SL.SaveToFile(NomeArquivo)
  Finally
    SL.Free;
  end;
end ;

procedure TACBrSAT.ExtrairLogs(AStringList: TStrings);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.Clear;
    ExtrairLogs(MS);
    MS.Seek(0,soBeginning);
    AStringList.LoadFromStream(MS);
  Finally
    MS.Free;
  end;
end;

procedure TACBrSAT.ExtrairLogs(AStream: TStream);
var
  LogBin : AnsiString;
begin
  fsComandoLog := 'ExtrairLogs';
  IniciaComando;
  FinalizaComando( fsSATClass.ExtrairLogs );

  // TODO: Criar verificação para os retornos: 15002, e 11098 - SAT em processamento

  // Transformando retorno, de Base64 para ASCII
  if (Resposta.RetornoLst.Count > 5) and
     (Resposta.codigoDeRetorno = 15000) then  // 1500 = Transferência completa
  begin
    LogBin := DecodeBase64( Resposta.RetornoLst[5] );

    AStream.Size     := 0;
    AStream.Position := 0;
    AStream.WriteBuffer(LogBin[1], Length(LogBin));
  end;
end;

function TACBrSAT.TesteFimAFim(dadosVenda : AnsiString) : String ;
var
  XMLRecebido, NomeCFe : String;
begin
  fsComandoLog := 'TesteFimAFim(' +dadosVenda+' )';
  IniciaComando;
  Result := FinalizaComando( fsSATClass.TesteFimAFim( dadosVenda ) );

  if fsResposta.codigoDeRetorno = 9000 then
  begin
     XMLRecebido := DecodeBase64(fsResposta.RetornoLst[5]);
     CFe.AsXMLString := XMLRecebido;

     if SalvarCFes then
     begin
       NomeCFe := PastaCFeVenda + PathDelim + CPREFIXO_CFe + CFe.infCFe.ID + '-teste.xml';
       WriteToTXT(NomeCFe, XMLRecebido, False, False);
     end;
  end;
end ;

function TACBrSAT.TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia: AnsiString;
  opcao: Integer; novoCodigo: AnsiString): String;
begin
  fsComandoLog := 'TrocarCodigoDeAtivacao('+ codigoDeAtivacao+', '+IntToStr(opcao)+
                  ', '+novoCodigo+' )';
  IniciaComando;
  Result := FinalizaComando( fsSATClass.TrocarCodigoDeAtivacao(
                                codigoDeAtivacaoOuEmergencia, opcao, novoCodigo ));
end ;

function TACBrSAT.GetAbout : String ;
begin
  Result := 'ACBrSAT Ver: '+CACBrSAT_Versao;
end ;

function TACBrSAT.GetcodigoDeAtivacao : AnsiString ;
var
  AcodigoDeAtivacao : AnsiString ;
begin
  AcodigoDeAtivacao := '';

  if Assigned( fsOnGetcodigoDeAtivacao ) then
     fsOnGetcodigoDeAtivacao( AcodigoDeAtivacao ) ;

  Result := AcodigoDeAtivacao;
end;

function TACBrSAT.GetModeloStrClass : String ;
begin
   Result := fsSATClass.ModeloStr;
end;

function TACBrSAT.GetsignAC : AnsiString ;
var
  AsignAC : AnsiString ;
begin
  AsignAC := '';

  if Assigned( fsOnGetsignAC ) then
     fsOnGetsignAC( AsignAC ) ;

  Result := AsignAC;
end;

procedure TACBrSAT.SetAbout(const Value : String) ;
begin
  {}
end ;

procedure TACBrSAT.SetInicializado(AValue : Boolean) ;
begin
  if AValue then
    Inicializar
  else
    DesInicializar ;
end ;

procedure TACBrSAT.SetModelo(AValue : TACBrSATModelo) ;
var
  wArqLOG : String ;
begin
  if fsModelo = AValue then exit ;

  if fsInicializado then
     raise EACBrSATErro.Create( cACBrSATSetModeloException );

  wArqLOG := ArqLOG ;

  FreeAndNil( fsSATClass ) ;

  { Instanciando uma nova classe de acordo com AValue }
  case AValue of
    satDinamico_cdecl : fsSATClass := TACBrSATDinamico_cdecl.Create( Self ) ;
    satDinamico_stdcall : fsSATClass := TACBrSATDinamico_stdcall.Create( Self ) ;
  else
    fsSATClass := TACBrSATClass.Create( Self ) ;
  end;

  { Passando propriedades da Classe anterior para a Nova Classe }
  ArqLOG := wArqLOG ;

  fsModelo := AValue;
end ;

procedure TACBrSAT.SetNomeDLL(AValue : string) ;
var
  FileName: String;
begin
  if fsNomeDLL = AValue then Exit ;
  fsNomeDLL := Trim(AValue) ;

  FileName := ExtractFileName( fsNomeDLL );
  if FileName = '' then
    fsNomeDLL := PathWithDelim( fsNomeDLL ) + cLIBSAT;
end ;

function TACBrSAT.GetPastaCFeCancelamento: String;
begin
  if fsPastaCFeCancelamento = '' then
     if not (csDesigning in Self.ComponentState) then
        fsPastaCFeCancelamento := ExtractFilePath( ParamStr(0) ) + 'CFesCancelados' ;

  Result := fsPastaCFeCancelamento ;
end;

function TACBrSAT.GetPastaCFeVenda: String;
begin
  if fsPastaCFeVenda = '' then
     if not (csDesigning in Self.ComponentState) then
        fsPastaCFeVenda := ExtractFilePath( ParamStr(0) ) + 'CFesEnviados' ;

  Result := fsPastaCFeVenda ;
end;

procedure TACBrSAT.SetPastaCFeCancelamento(AValue: String);
begin
  fsPastaCFeCancelamento := PathWithoutDelim( AValue );
end;

procedure TACBrSAT.SetPastaCFeVenda(AValue: String);
begin
  fsPastaCFeVenda := PathWithoutDelim( AValue );
end;

procedure TACBrSAT.SetExtrato(const Value: TACBrSATExtratoClass);
Var
  OldValue: TACBrSATExtratoClass ;
begin
  if Value <> fsExtrato then
  begin
     if Assigned(fsExtrato) then
        fsExtrato.RemoveFreeNotification(Self);

     OldValue  := fsExtrato ;   // Usa outra variavel para evitar Loop Infinito
     fsExtrato := Value;    // na remoção da associação dos componentes

     if Assigned(OldValue) then
        if Assigned(OldValue.ACBrSAT) then
           OldValue.ACBrSAT := nil ;

     if Value <> nil then
     begin
        Value.FreeNotification(self);
        Value.ACBrSAT := self ;
     end ;
  end ;
end;

procedure TACBrSAT.Notification(AComponent : TComponent ; Operation : TOperation
  ) ;
begin
  inherited Notification(AComponent, Operation) ;

  if (Operation = opRemove) and (fsExtrato <> nil) and (AComponent is TACBrSATExtratoClass) then
     fsExtrato := nil ;
end ;

procedure TACBrSAT.CFe2CFeCanc;
begin
  CFeCanc.Clear;
  CFeCanc.infCFe.chCanc   := fsPrefixoCFe + CFe.infCFe.ID;
  CFeCanc.infCFe.dEmi     := CFe.ide.dEmi;
  CFeCanc.infCFe.hEmi     := CFe.ide.hEmi;
  CFeCanc.ide.CNPJ        := CFe.ide.CNPJ;
  CFeCanc.ide.signAC      := CFe.ide.signAC;
  CFeCanc.ide.numeroCaixa := CFe.ide.numeroCaixa;
  CFeCanc.Dest.CNPJCPF    := CFe.Dest.CNPJCPF;
end;

procedure TACBrSAT.ImprimirExtrato;
begin
  VerificaCondicoesImpressao;
  Extrato.ImprimirExtrato;
end;

procedure TACBrSAT.ImprimirExtratoResumido;
begin
  VerificaCondicoesImpressao;
  Extrato.ImprimirExtratoResumido;
end;

procedure TACBrSAT.ImprimirExtratoCancelamento;
begin
  VerificaCondicoesImpressao;
  Extrato.ImprimirExtratoCancelamento;
end;

function TACBrSAT.CodificarPaginaDeCodigoSAT(ATexto: String): AnsiString;
begin
  if fsConfig.PaginaDeCodigo > 0 then
     Result := TranslateString( ACBrStrToAnsi( ATexto ), fsConfig.PaginaDeCodigo )
  else
     Result := TiraAcentos( ATexto );
end ;

function TACBrSAT.DecodificarPaginaDeCodigoSAT(ATexto : AnsiString
   ) : String ;
begin
  if fsConfig.PaginaDeCodigo > 0 then
     Result := ACBrStr( TranslateString( ATexto, 0, fsConfig.PaginaDeCodigo ) )
  else
     Result := ACBrStr( ATexto ) ;
end ;

end.

