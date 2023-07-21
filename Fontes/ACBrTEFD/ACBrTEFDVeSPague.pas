{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

{$I ACBr.inc}

unit ACBrTEFDVeSPague;

interface

uses
  Classes, SysUtils, ACBrTEFDClass, blcksock
  {$IfNDef NOGUI}
    {$If DEFINED(VisualCLX)}
      ,QControls, QForms
    {$ElseIf DEFINED(FMX)}
      ,System.UITypes, FMX.Forms
    {$ElseIf DEFINED(DELPHICOMPILER16_UP)}
      ,System.UITypes
    {$Else}
      ,Controls, Forms
    {$IfEnd}
  {$EndIf};


Const
   CACBrTEFD_VeSPague_Backup = 'ACBr_VeSPague_Backup.tef' ;
   CACBrTEFD_VeSPague_Terminador = #13+#10+#09+#09+#13+#10+#09+#09+#09+#13+#10+#09+#09+#13+#10+#09 ;

type

  { TACBrTEFDVeSPagueCmd }

  TACBrTEFDVeSPagueCmd = class
  private
    fsIsColeta : Boolean ;
    fsParams : TStringList ;
    fsSequencial : Integer ;
    function GetFrameEnvio : AnsiString ;
    function GetRetorno : Integer ;
    function GetSequencial : Integer ;
    function GetServico : AnsiString ;
    procedure SetFrameEnvio(const AValue : AnsiString) ;
    procedure SetIsColeta(const AValue : Boolean) ;
    procedure SetRetorno(const AValue : Integer) ;
    procedure SetSequencial(const AValue : Integer) ;
    procedure SetServico(const AValue : AnsiString) ;
  public
    constructor Create ;
    destructor Destroy ; override ;

    property IsColeta    : Boolean     read fsIsColeta write SetIsColeta ;
    property Sequencial  : Integer     read GetSequencial write SetSequencial ;
    property Servico     : AnsiString  read GetServico write SetServico ;
    property Retorno     : Integer     read GetRetorno write SetRetorno ;

    property FrameEnvio  : AnsiString  read GetFrameEnvio write SetFrameEnvio ;
    property Params      : TStringList read fsParams ;

    procedure Clear ;

    Function GetParamString(const ParamName : String) : AnsiString ;
    Function GetParamInteger(const ParamName : String) : Integer ;
    Function GetParamDouble(const ParamName : String) : Double ;
    Function GetParamDateTime(const ParamName : String) : TDateTime ;
    Procedure GetParamStrings(const ParamName : String; const AStringList : TStrings) ;

    Procedure AddParamString(const ParamName : String; const AString  : AnsiString) ;
    Procedure AddParamInteger(const ParamName : String; const AInteger : Integer) ;
    Procedure AddParamDouble(const ParamName : String; const ADouble  : Double) ;
    Procedure AddParamDateTime(const ParamName : String;
       const ADateTime: TDateTime; const Tipo : AnsiChar = 'D'  ) ;
    Procedure AddParamStrings(const ParamName : String; const AStringList : TStrings) ;
  end ;

  { TACBrTEFDRespVeSPague }

  TACBrTEFDRespVeSPague = class( TACBrTEFDResp )
  protected
    function GetTransacaoAprovada : Boolean; override;
  public
    procedure ConteudoToProperty; override;
    procedure GravaInformacao( const PalavraChave, Informacao : AnsiString ) ;
  end;

  TACBrTEFDVeSPagueExibeMenu = procedure( Titulo : String; Opcoes : TStringList;
     Memo: TStringList; var ItemSelecionado : Integer) of object ;

  TACBrTEFDVeSPagueObtemCampo = procedure( Titulo : String;
    Mascara : String; Tipo : AnsiChar; var Resposta : String;
    var Digitado : Boolean ) of object ;

  { TACBrTEFDVeSPague }

   TACBrTEFDVeSPague = class( TACBrTEFDClass )
   private
      fAplicacao       : String;
      fAplicacaoVersao : String;
      fComputadorEndereco: string;
      fComputadorNome: string;
      fEstabelecimento: string;
      fLoja: string;
      fTemPendencias   : Boolean;
      fCancelandoTransacao: Boolean;
      fGPExeParams : String ;

      fTerminador : AnsiString ;
      fEnderecoIP : AnsiString;
      fPorta      : AnsiString;
      fTerminal: string;
      fTimeOut    : Integer ;

      fSocket : TTCPBlockSocket ;
      fReqVS  : TACBrTEFDVeSPagueCmd ;
      fRespVS : TACBrTEFDVeSPagueCmd ;

      fOnExibeMenu : TACBrTEFDVeSPagueExibeMenu;
      fOnObtemCampo : TACBrTEFDVeSPagueObtemCampo;
      fTransacaoADM : String;
      fTransacaoCHQ : String;
      fTransacaoCNC : String;
      fTransacaoCRT : String;
      fTransacaoOpcao : String ;
      fTransacaoPendente : String ;
      fTransacaoReImpressao: String;
      fTransacaoPix: String;

     procedure ExecutarTranscaoPendente(NSU : String ; Valor : Double) ;
     procedure FinalizarTranscaoPendente;
     procedure SeTimeOut(const AValue : Integer) ;
     procedure TransmiteCmd ;

     procedure ProcessarColeta ;

     procedure ServicoIniciar ;
     procedure ServicoFinalizar ;

     procedure AvaliaErro( Retorno : Integer) ;
   protected
     procedure SetNumVias(const AValue : Integer); override;

     function FazerRequisicao(Transacao: String; AHeader : AnsiString = '' ;
       Valor : Double = 0 ; Documento : AnsiString = '';
        ListaParams : AnsiString = '') : Integer ;
     Function ContinuarRequisicao( ImprimirComprovantes : Boolean = True) : Integer ;
     procedure FinalizarRequisicao ; override ;

     procedure ProcessarResposta ; override ;
     Function ProcessarRespostaPagamento( const IndiceFPG_ECF : String;
        const Valor : Double) : Boolean; override;

     Function Conectar : Integer ;
     Function DesConectar : Integer ;

   public
     constructor Create( AOwner : TComponent ) ; override;
     destructor Destroy ; override;

     property Socket : TTCPBlockSocket read fSocket ;
     property ReqVS  : TACBrTEFDVeSPagueCmd read fReqVS  ;
     property RespVS : TACBrTEFDVeSPagueCmd read fRespVS ;
     property Terminador : AnsiString read fTerminador write fTerminador ;

     procedure Inicializar ; override;
     procedure DesInicializar ; override;

     procedure AtivarGP ; override;
     procedure VerificaAtivo ; override;

     procedure VerificarTransacoesPendentesClass(aVerificarCupom: Boolean);
       override;

     Procedure ATV ; override;
     Function ADM : Boolean ; override;
     Function CRT( Valor : Double; IndiceFPG_ECF : String;
        DocumentoVinculado : String = ''; Moeda : Integer = 0 ) : Boolean; override;
     Function CHQ( Valor : Double; IndiceFPG_ECF : String;
        DocumentoVinculado : String = ''; CMC7 : String = '';
        TipoPessoa : AnsiChar = 'F'; DocumentoPessoa : String = '';
        DataCheque : TDateTime = 0; Banco   : String = '';
        Agencia    : String = ''; AgenciaDC : String = '';
        Conta      : String = ''; ContaDC   : String = '';
        Cheque     : String = ''; ChequeDC  : String = '';
        Compensacao: String = '' ) : Boolean ; override;
     Procedure NCN(Rede, NSU, Finalizacao : String;
        Valor : Double = 0; DocumentoVinculado : String = ''); override;
     Procedure CNF(Rede, NSU, Finalizacao : String;
        DocumentoVinculado : String = ''); override;
     Function CNC(Rede, NSU : String; DataHoraTransacao : TDateTime;
        Valor : Double; CodigoAutorizacaoTransacao: String = '') : Boolean; overload; override;

     function ObtemDadosPinPad(pTipoDocumento: String = 'CPF'): String;
   published
     property Aplicacao       : String read fAplicacao       write fAplicacao ;
     property AplicacaoVersao : String read fAplicacaoVersao write fAplicacaoVersao ;

     property GPExeName;
     property GPExeParams : String read fGPExeParams write fGPExeParams ;

     property EnderecoIP : AnsiString read fEnderecoIP  write fEnderecoIP ;
     property Porta      : AnsiString read fPorta       write fPorta ;
     property TimeOut    : Integer    read fTimeOut     write SeTimeOut default 1000 ;
     Property TemPendencias: Boolean read fTemPendencias write fTemPendencias;

     property Estabelecimento: string read fEstabelecimento write fEstabelecimento;
     property Loja: string read fLoja write fLoja;
     property Terminal: string read fTerminal write fTerminal;
     property ComputadorNome: string read fComputadorNome write fComputadorNome;
     property ComputadorEndereco: string read fComputadorEndereco write fComputadorEndereco;

     property TransacaoADM   : String read fTransacaoADM   write fTransacaoADM ;
     property TransacaoCRT   : String read fTransacaoCRT   write fTransacaoCRT ;
     property TransacaoCHQ   : String read fTransacaoCHQ   write fTransacaoCHQ ;
     property TransacaoCNC   : String read fTransacaoCNC   write fTransacaoCNC ;
     property TransacaoOpcao : String read fTransacaoOpcao write fTransacaoOpcao ;
     property TransacaoReImpressao : String read fTransacaoReImpressao
        write fTransacaoReImpressao ;
     property TransacaoPendente : String read fTransacaoPendente
        write fTransacaoPendente ;
     property TransacaoPix: String read fTransacaoPix write fTransacaoPix;

     property OnExibeMenu : TACBrTEFDVeSPagueExibeMenu read fOnExibeMenu
        write fOnExibeMenu ;
     property OnObtemCampo : TACBrTEFDVeSPagueObtemCampo read fOnObtemCampo
        write fOnObtemCampo ;
   end;

function DecodificaString(const AString : AnsiString) : AnsiString ;
function DateTimeToVSDateTime( ADateTime : TDateTime; Tipo : AnsiChar = 'D') : String ;
function VSDateTimeToDateTime(const AVSDateTime : String) : TDateTime ;
procedure VSStringToList( const AString : AnsiString; const AList : TStrings) ;

implementation

Uses
  strutils, math, dateutils,
  ACBrUtil.Strings,
  ACBrUtil.Math,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrTEFD, ACBrTEFComum;

{ TACBrTEFDVeSPagueCmd }

function DateTimeToVSDateTime( ADateTime : TDateTime; Tipo : AnsiChar) : String ;
var
  Formato : String ;
begin
  Formato := '' ;

  case upcase(Tipo) of
    'T','H' : Formato := 'hh:nn:ss.zzz';
    'D'     : Formato := 'yyyy-mm-dd';
  else
    Formato := 'yyyy-mm-dd hh:nn:ss.zzz' ;
  end ;

  Result := FormatDateTime( Formato, ADateTime ) ;
end;

function VSDateTimeToDateTime(const AVSDateTime : String) : TDateTime ;
Var
  P, Ano, Mes, Dia, Hora, Min, Seg, Mili : Integer ;
begin
  Ano    := 0 ;
  Mes    := 0 ;
  Dia    := 0 ;
  Hora   := 0 ;
  Min    := 0 ;
  Seg    := 0 ;
  Mili   := 0 ;

  try
     P := pos('-',AVSDateTime) ;
     if (P > 0) then   // Tem Data com seprador - ?
      begin
        Ano := StrToInt(Copy(AVSDateTime,1,4));
        Mes := StrToInt(Copy(AVSDateTime,6,2));
        Dia := StrToInt(Copy(AVSDateTime,9,2));
      end
     else
      begin
        P := pos('/',AVSDateTime) ;
        if (P > 0) then   // Tem Data com seprador / ?
        begin
           Dia := StrToInt(Copy(AVSDateTime,1,2));
           Mes := StrToInt(Copy(AVSDateTime,4,2));
           Ano := StrToInt(Copy(AVSDateTime,7,4));
        end ;
      end ;

     P := pos(':',AVSDateTime) ;
     if (P > 0) then   // Tem Hora ?
     begin
        Hora := StrToInt(Copy(AVSDateTime,P-2,2));
        Min  := StrToInt(Copy(AVSDateTime,P+1,2));
        Seg  := StrToInt(Copy(AVSDateTime,P+4,2));
        Mili := StrToInt(Copy(AVSDateTime,P+7,3));
     end ;

     Result := EncodeDateTime(Ano,Mes,Dia,Hora,Min,Seg,Mili);
  except
     Result := 0 ;
  end ;
end ;

procedure VSStringToList( const AString : AnsiString; const AList : TStrings) ;
var
  I : Integer ;
  Buffer : AnsiString ;
begin
  Buffer := AString;
  Buffer := StringReplace( Buffer, '";"', '"' + sLineBreak + '"', [rfReplaceAll]) ;

  AList.Clear ;
  AList.Text := Buffer ;

  for I := 0 to AList.Count - 1 do
     AList[I] := ACBrStr( DecodificaString(AList[I]) ) ;
end ;


function DecodificaString(const AString : AnsiString) : AnsiString ;
var
  Inicio, Tamanho : Integer ;
begin
  Result  := Trim(AString) ;
  Inicio  := 1 ;
  Tamanho := Length(Result) ;
  if LeftStr(Result,1) = '"' then
  begin
     Inicio := 2;
     Dec( Tamanho ) ;
  end ;

  if RightStr(Result,1) = '"' then
     Dec( Tamanho )  ;

  Result := Copy(Result, Inicio, Tamanho);
  Result := StringToBinaryString(Result);
end ;

{ TACBrTEFDVeSPagueCmd }

constructor TACBrTEFDVeSPagueCmd.Create ;
begin
  inherited ;
  fsParams := TStringList.Create;
  Clear;
end ;

destructor TACBrTEFDVeSPagueCmd.Destroy ;
begin
  fsParams.Free ;

  inherited ;
end ;

procedure TACBrTEFDVeSPagueCmd.Clear ;
begin
  fsParams.Clear;
  fsSequencial := 0 ;
  fsIsColeta   := False ;
end ;

procedure TACBrTEFDVeSPagueCmd.GetParamStrings(const ParamName : String ;
  const AStringList : TStrings) ;
begin
  VSStringToList( fsParams.Values[ParamName], AStringList) ;
end ;

function TACBrTEFDVeSPagueCmd.GetFrameEnvio : AnsiString ;
begin
  Result := fsParams.Text;
  Result := StringToBinaryString(Result);
end;

procedure TACBrTEFDVeSPagueCmd.SetFrameEnvio(const AValue : AnsiString) ;
Var
  Buffer : AnsiString;
begin
  fsParams.Clear;
  fsSequencial := 0 ;

  Buffer := StringReplace(AValue, CR+LF, LF, [rfReplaceAll]);
  Buffer := StringReplace(Buffer, '="'+LF, '="\x0A', [rfReplaceAll]);
  Buffer := StringReplace(Buffer,'"'+LF,'"[LineBreak]', [rfReplaceAll]);
  Buffer := BinaryStringToString(Buffer);
  Buffer := StringReplace(Buffer,'[LineBreak]',sLineBreak,[rfReplaceAll]);

  fsParams.Text := Buffer;
  fsIsColeta    := (GetParamInteger('automacao_coleta_sequencial') > 0);
end;

procedure TACBrTEFDVeSPagueCmd.SetIsColeta(const AValue : Boolean) ;
begin
  if AValue = fsIsColeta then exit ;

  fsIsColeta := AValue;
end;

function TACBrTEFDVeSPagueCmd.GetRetorno : Integer ;
begin
  Result := GetParamInteger( ifthen(IsColeta,'automacao_coleta_retorno', 'retorno') );
end;

function TACBrTEFDVeSPagueCmd.GetSequencial : Integer ;
begin
  if fsSequencial = 0 then
     fsSequencial := GetParamInteger( ifthen(IsColeta,'automacao_coleta_sequencial',
                                                      'sequencial') );

  Result := fsSequencial;
end;

function TACBrTEFDVeSPagueCmd.GetServico : AnsiString ;
begin
  Result := GetParamString('servico')
end;

function TACBrTEFDVeSPagueCmd.GetParamString(const ParamName : String) : AnsiString ;
begin
   Result := DecodificaString( fsParams.Values[ParamName] );
end ;

function TACBrTEFDVeSPagueCmd.GetParamInteger(const ParamName : String) : Integer ;
begin
  Result := StrToInt64Def(GetParamString(ParamName),0);
end ;

function TACBrTEFDVeSPagueCmd.GetParamDouble(const ParamName : String) : Double ;
begin
  Result := StringToFloatDef(GetParamString(ParamName),0);
end ;

function TACBrTEFDVeSPagueCmd.GetParamDateTime(const ParamName : String) : TDateTime ;
begin
  Result := VSDateTimeToDateTime( GetParamString(ParamName) ) ;
end ;

procedure TACBrTEFDVeSPagueCmd.SetRetorno(const AValue : Integer) ;
begin
   AddParamInteger(ifthen(IsColeta,'automacao_coleta_retorno','retorno'),AValue);
end;

procedure TACBrTEFDVeSPagueCmd.SetSequencial(const AValue : Integer) ;
begin
  fsSequencial := AValue;
  AddParamInteger(ifthen(IsColeta,'automacao_coleta_sequencial','sequencial'),AValue);
end;

procedure TACBrTEFDVeSPagueCmd.SetServico(const AValue : AnsiString) ;
begin
  fsParams.Clear;
  fsIsColeta := False ;
  Sequencial := fsSequencial + 1;
  Retorno    := 1 ;

  AddParamString('servico',AValue);
end;

procedure TACBrTEFDVeSPagueCmd.AddParamString(const ParamName: String;
  const AString: AnsiString);
begin
   fsParams.Values[ParamName] := '"'+AString+'"';
end ;

procedure TACBrTEFDVeSPagueCmd.AddParamInteger(const ParamName : String ;
  const AInteger : Integer) ;
begin
   AddParamString(ParamName, IntToStr(AInteger)  );
end ;

procedure TACBrTEFDVeSPagueCmd.AddParamDouble(const ParamName : String ;
  const ADouble : Double) ;
Var
  StrValue : String ;
begin
   StrValue := FloatToString(ADouble);
   AddParamString(ParamName,StrValue);
end ;

procedure TACBrTEFDVeSPagueCmd.AddParamDateTime(const ParamName : String ;
  const ADateTime : TDateTime ; const Tipo : AnsiChar) ;
begin
   AddParamString(ParamName, DateTimeToVSDateTime( ADateTime, Tipo) );
end ;

procedure TACBrTEFDVeSPagueCmd.AddParamStrings(const ParamName : String ;
  const AStringList : TStrings) ;
Var
  Buffer : AnsiString ;
  I      : Integer ;
begin
  Buffer := '' ;

  For I := 0 to AStringList.Count-1 do
     Buffer := Buffer + '"'+ AStringList[I] +'";' ;

  Buffer := copy(Buffer,1,Length(Buffer)-1);  // Remove o ultimo ";"

  AddParamString( ParamName, Buffer );
end ;

{ TACBrTEFDRespVeSPague }

function TACBrTEFDRespVeSPague.GetTransacaoAprovada : Boolean;
begin
   Result := True ;
end;

procedure TACBrTEFDRespVeSPague.ConteudoToProperty;
var
  Linha : TACBrTEFLinha ;
  Chave, ParcValorStr, ParcVenctoStr : AnsiString;
  Valor : String ;
  I : Integer ;
  ParcValorList, ParcVenctoList : TStringList ;
  Parc : TACBrTEFRespParcela ;
begin
   fpValorTotal := 0 ;
   ParcVenctoStr:= '';
   ParcValorStr := '';
   fpImagemComprovante1aVia.Clear;
   fpImagemComprovante2aVia.Clear;

   for I := 0 to Conteudo.Count - 1 do
   begin
     Linha := Conteudo.Linha[I];

     Chave := LowerCase( Linha.Chave );
     Valor := StringToBinaryString( Linha.Informacao.AsString );

     if Chave = 'transacao_administradora' then
     begin
        fpNFCeSAT.Bandeira := Linha.Informacao.AsString;
        fpNomeAdministradora := Linha.Informacao.AsString;
        fpRede := Linha.Informacao.AsString ;
     end
     else if Chave = 'transacao_autorizacao' then
     begin
        fpNFCeSAT.Autorizacao := Linha.Informacao.AsString;
        fpCodigoAutorizacaoTransacao := Linha.Informacao.AsString;
     end
     else if Chave = 'transacao_codigo_vespague' then
        fpNumeroLoteTransacao := Linha.Informacao.AsInteger
     else if Chave = 'transacao_comprovante_1via' then
        fpImagemComprovante1aVia.Text := StringToBinaryString( Linha.Informacao.AsString )
     else if Chave = 'transacao_comprovante_2via' then
        fpImagemComprovante2aVia.Text := StringToBinaryString( Linha.Informacao.AsString )
     else if Chave = 'transacao_comprovante_resumido' then
        // Ainda Não mepeado
     else if Chave = 'transacao_data' then
        fpDataHoraTransacaoComprovante := VSDateTimeToDateTime( Valor )
     else if Chave = 'transacao_financiado' then
        fpTipoParcelamento := ifthen(LowerCase(Valor)='estabelecimento', 0, 1 )
     else if Chave = 'transacao_nsu' then
        fpNSU := Linha.Informacao.AsString
     else if Chave = 'transacao_pagamento' then
        fpModalidadePagto := Valor
     else if Chave = 'transacao_parcela' then
        fpQtdParcelas := Linha.Informacao.AsInteger
     else if Chave = 'transacao_parcela_valor' then
        ParcValorStr := Valor
     else if Chave = 'transacao_parcela_vencimento' then
        ParcVenctoStr := Valor
     else if Chave = 'transacao_produto' then
        fpModalidadePagtoDescrita := Linha.Informacao.AsString
     else if Chave = 'transacao_tipo_cartao' then
     begin
        fpTipoTransacao := ifthen(LowerCase(Valor)='debito', 20, 10 );

        fpDebito  := (LowerCase(Valor) = 'debito');
        fpCredito := (LowerCase(Valor) = 'credito');
     end
     else if Chave = 'transacao_valor' then
        fpValorTotal := StringToFloatDef( Valor, 0 )
     else if Chave = 'transacao_valor_ajuste' then //---Valor retornado contendo o valor CIELO PREMIA
        fpDesconto := StringToFloatDef( Valor, 0 )
     else if Chave = 'transacao_valor_saque' then
        fpSaque := StringToFloatDef( Valor, 0 )
     else if Chave = 'transacao_valor_taxa_embarque' then
        // Ainda Não mepeado
     else if Chave = 'transacao_valor_taxa_servico' then
        // Ainda Não mepeado
     else if Chave = 'transacao_vencimento' then
        fpDataVencimento := VSDateTimeToDateTime( Valor )
     else if Linha.Identificacao = 27 then
       fpFinalizacao := Valor
     else if Chave = 'estabelecimento' then
       fpEstabelecimento := Linha.Informacao.AsString
     else if Chave = 'transacao_rede_cnpj' then
       fpNFCeSAT.CNPJCredenciadora := Linha.Informacao.AsString
     else
       ProcessarTipoInterno(Linha);
   end ;

   fpQtdLinhasComprovante := fpImagemComprovante1aVia.Count;
   if fpQtdLinhasComprovante = 0 then
      fpQtdLinhasComprovante := fpImagemComprovante2aVia.Count;

   if (ParcVenctoStr <> '') and (ParcValorStr <> '') then
   begin
     ParcValorList  := TStringList.Create;
     ParcVenctoList := TStringList.Create;
     try
       fpParcelas.Clear;

       VSStringToList( ParcValorStr, ParcValorList ) ;
       VSStringToList( ParcVenctoStr,ParcVenctoList ) ;

       if ParcValorList.Count = ParcVenctoList.Count then
       begin
         for I := 1 to ParcValorList.Count-1 do
         begin
            Parc := TACBrTEFRespParcela.create;
            Parc.Vencimento := VSDateTimeToDateTime( ParcVenctoList[I] );
            Parc.Valor      := StringToFloatDef( ParcValorList[I], 0) ;

            fpParcelas.Add(Parc);
         end;
       end ;
     finally
        ParcValorList.Free ;
        ParcVenctoList.Free ;
     end ;
   end ;
end;

procedure TACBrTEFDRespVeSPague.GravaInformacao( const PalavraChave,
   Informacao : AnsiString );
begin
  fpConteudo.GravaInformacao( PalavraChave,
     BinaryStringToString( DecodificaString(Informacao) ) ); // Converte #10 para "\x0A"
end;


{ TACBrTEFDClass }

constructor TACBrTEFDVeSPague.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ArqReq    := '' ;
  ArqResp   := '' ;
  ArqSTS    := '' ;
  ArqTemp   := '' ;
  GPExeName := '' ;
  fpTipo    := gpVeSPague;
  Name      := 'VeSPague' ;

  fEnderecoIP := 'localhost' ;
  fPorta      := '60906' ;
  fTimeOut    := 1000 ;
  fTerminador := CACBrTEFD_VeSPague_Terminador ;
  fTemPendencias := False;

  fAplicacao       := '' ;
  fAplicacaoVersao := '' ;

  fSocket := nil ;
  fReqVS  := TACBrTEFDVeSPagueCmd.Create;
  fRespVS := TACBrTEFDVeSPagueCmd.Create;

  fTransacaoCRT         := 'Cartao Vender' ;
  fTransacaoReImpressao := 'Administracao Reimprimir' ;
  fTransacaoPendente    := 'Administracao Pendente' ;
  fTransacaoADM         := '' ;
  fTransacaoCHQ         := 'Cheque Consultar' ;
  fTransacaoCNC         := 'Administracao Cancelar' ;
  fTransacaoOpcao       := '' ;

  fOnExibeMenu  := nil ;
  fOnObtemCampo := nil ;

  if Assigned( fpResp ) then
     fpResp.Free ;

  fpResp := TACBrTEFDRespVeSPague.Create;
  fpResp.TipoGP := Tipo;
end;

destructor TACBrTEFDVeSPague.Destroy;
begin
  if Assigned(fSocket) then
  begin
    fSocket.CloseSocket;
    fSocket.Free;
  end ;

  fReqVS.Free ;
  fRespVS.Free ;

  inherited Destroy;
end;

procedure TACBrTEFDVeSPague.SetNumVias(const AValue : Integer);
begin
   fpNumVias := 2;
end;

procedure TACBrTEFDVeSPague.SeTimeOut(const AValue : Integer) ;
begin
  if AValue = fTimeOut then exit ;

  if AValue < 100 then
     raise EACBrTEFDErro.Create( ACBrStr('Valor mínimo deve ser 100') ) ;

  fTimeOut := AValue;
end;

function TACBrTEFDVeSPague.Conectar: Integer;
begin
  if not Assigned(fSocket) then
     fSocket := TTCPBlockSocket.Create;

  fSocket.CloseSocket;
  fSocket.Connect( fEnderecoIP, fPorta );
  Result := fSocket.LastError ;
end ;

function TACBrTEFDVeSPague.DesConectar: Integer;
begin
  fSocket.CloseSocket;
  Result := fSocket.LastError ;
end ;

procedure TACBrTEFDVeSPague.Inicializar;
var
  Erro, Tentativas : Integer ;
begin
  if Inicializado then exit ;

  if not Assigned( OnExibeMenu ) then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnExibeMenu" não programado' ) ) ;

  if not Assigned( OnObtemCampo ) then
     raise EACBrTEFDErro.Create( ACBrStr('Evento "OnObtemCampo" não programado' ) ) ;

  Erro := Conectar;
  if (Erro = 10061) and AutoAtivarGP and (GPExeName <> '') and (GPExeParams <> '') then
  begin
     AtivarGP;

     Tentativas := 0 ;
     while (Erro = 10061) and (Tentativas < 10) do // 10061 = Connection Refused
     begin
        Sleep(1000) ;
        GravaLog( 'Tentativa de conexão '+IntToStr(Tentativas) );

        DesConectar;
        Erro := Conectar;

        Inc( Tentativas ) ;
     end ;
  end ;

  if Erro <> 0 then
     raise EACBrTEFDErro.Create( ACBrStr('Erro ao conectar no V&SPague'+sLineBreak+
                             'Endereço: '+fEnderecoIP+sLineBreak+
                             'Porta: '+fPorta+sLineBreak+
                             'Erro: '+IntToStr(Erro)+'-'+fSocket.LastErrorDesc ) ) ;

  GravaLog( Name +' Inicializado VeSPague' );

  ServicoIniciar;

  VerificarTransacoesPendentesClass(True);
  fpInicializado := True;
end;

procedure TACBrTEFDVeSPague.DesInicializar ;
begin
  DesConectar;

  inherited DesInicializar ;
end ;

procedure TACBrTEFDVeSPague.AtivarGP ;
begin
  if (GPExeName = '') then
     raise EACBrTEFDErro.Create(ACBrStr('Linha de comando de execuçao do V&SPague Cliente não definida'));

  if (GPExeParams = '') then
     raise EACBrTEFDErro.Create(ACBrStr('Parametros de Execução para o V&SPague Cliente não informados'+sLineBreak+
                                        'Especifique: IPSerividor Porta  Exemplo:'+sLineBreak+
                                        '200.111.222.333 65432'));

  GravaLog( 'Ativando V&SPague Client: ' +GPExeName+' '+GPExeParams );

  RunCommand( GPExeName, GPExeParams );
  Sleep(2000);
  TACBrTEFD(Owner).RestaurarFocoAplicacao;
end ;

procedure TACBrTEFDVeSPague.VerificaAtivo ;
begin
  if Inicializado then
     Inherited VerificaAtivo
  else
     Inicializar;
end ;

procedure TACBrTEFDVeSPague.VerificarTransacoesPendentesClass(
  aVerificarCupom: Boolean);
var
  wEstadoECF: AnsiChar;
  ArqMask: String;
begin
  try
    wEstadoECF := TACBrTEFD(Owner).EstadoECF;
  except
    wEstadoECF := 'O';
    { TODO: Criar arquivo de Status da Transação

        Se o ECF estiver desligado, será retornado 'O', o que fará o código
      abaixo Cancelar Todas as Transações Pendentes, porém, pelo Roteiro do
      TEF dedicado, é necessário confirmar a Transação se o Cupom foi
      finalizado com sucesso.
        Criar um arquivo de Status que seja atualizado no Fim do Cupom e no
      inicio do CCD, de maneira que seja possível identificar o Status do
      Documento no ECF indepentende do mesmo estar ou não ligado

        Como alteranativa, é possível implementar código no Evento "OnInfoECF"
      para buscar o Status do Documento no Banco de dados da sua aplicação, e
      responder diferente de 'O',   (Veja exemplo nos fontes do TEFDDemo)
    }
  end;

  // Cupom Ficou aberto ?? Se SIM, Cancele tudo...
  if (wEstadoECF in ['V','P','N','O']) then
  begin
    { Achando Arquivos de Backup deste GP }
    ArqMask := TACBrTEFD(Owner).PathBackup + PathDelim + 'ACBr_' + Self.Name + '_*.tef';
    if FilesExists(ArqMask) then
    begin
      TACBrTEFD(Owner).DoExibeMsg(opmOK, 'Há pelo menos uma transação PENDENTE.' +
        sLineBreak + 'Favor realizar o DESFAZIMENTO no menu Administrativo -> pedende.' +
        sLineBreak + 'Cancelar o cupom fiscal!');

      CancelarTransacoesPendentesClass;
    end;
  end
  else
    // NAO, Cupom Fechado, Pode confirmar e Mandar aviso para re-imprimir
    ConfirmarESolicitarImpressaoTransacoesPendentes;
end;

procedure TACBrTEFDVeSPague.ATV;
begin
  ServicoIniciar;
  ServicoFinalizar;
end;

function TACBrTEFDVeSPague.ObtemDadosPinPad(pTipoDocumento: String = 'CPF'): String;
var
  vDadosPinpad: AnsiString;
begin
  Result := '';
  ServicoIniciar;
  ReqVS.Servico := 'coletar' ;
  ReqVS.AddParamString( 'mensagem', pTipoDocumento + 'E' ) ; // CPF segundo manual
  TransmiteCmd;
  if RespVS.Retorno = 1 then
  begin
    vDadosPinpad := RespVS.GetParamString('transacao_informacao');

    ReqVS.Servico := 'perguntar' ;
    ReqVS.AddParamString( 'mensagem', pTipoDocumento + 'C+' + vDadosPinpad );
    TransmiteCmd;

    if RespVS.Retorno = 1 then
    begin
      Result := vDadosPinpad;
    end;
  end ;

  ReqVS.Params.Clear;
  RespVS.Params.Clear;
  ReqVS.Clear;
  RespVS.Clear;

  ServicoFinalizar;
end;

function TACBrTEFDVeSPague.ADM: Boolean;
var
  Retorno : Integer ;
  SL1, SL2 : TStringList ;
  I : Integer ;
  Transacao : String ;
begin
  Result := True ;
  Transacao := Trim(fTransacaoADM) ;

  { No modo BackGround não há um comando que retorne todas as Opçoes
    Administrativas (sic) ... então vamos cria-lo ;) }
  if Transacao = '' then
  begin
    ServicoIniciar;
    ReqVS.Servico := 'consultar' ;
    TransmiteCmd;

    if RespVS.Retorno = 1 then
    begin
       SL1 := TStringList.Create;
       SL2 := TStringList.Create;
       try
          SL2.Clear;
          RespVS.GetParamStrings('transacao',SL1);
          For I := 0 to SL1.Count-1 do
          begin
             if pos(copy(SL1[I],1,6), 'Cartao|Cheque') = 0 then
                SL2.Add(SL1[I]);
          end ;
          SL1.Clear;

          if SL2.Count > 0 then
          begin
             SL2.Sort;
             TACBrTEFD(Owner).BloquearMouseTeclado(False);
             OnExibeMenu( ACBrStr('Escolha a Transação'), SL2, SL1, I ) ;

             if (I >= 0) and (I < SL2.Count) then
                Transacao := SL2[ I ] ;
          end ;
       finally
          SL1.Free;
          SL2.Free;
       end ;
    end ;
  end ;

  if Transacao <> '' then
  begin
     Retorno := FazerRequisicao( Transacao, 'ADM' ) ;

     if Retorno in [0,1] then
        Retorno := ContinuarRequisicao( True ) ;  { True = Imprimir Comprovantes agora }

     Result := ( Retorno in [0,1] ) ;

     ProcessarResposta ;         { Faz a Impressão e / ou exibe Mensagem ao Operador }
     ServicoFinalizar;
  end ;

  ReqVS.Params.Clear;
  RespVS.Params.Clear;
  ReqVS.Clear;
  RespVS.Clear;
end;

function TACBrTEFDVeSPague.CRT(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; Moeda: Integer): Boolean;
var
  Retorno : Integer ;
begin
  VerificarTransacaoPagamento( Valor );

  if LowerCase(fTransacaoPix) = 'digital pagar' then
    Retorno := FazerRequisicao('Digital Pagar', 'CRT', Valor, DocumentoVinculado)
  else
    Retorno := FazerRequisicao(fTransacaoCRT, 'CRT', Valor, DocumentoVinculado);

  if Retorno = 0 then
     Retorno := ContinuarRequisicao( False ) ;  { False = NAO Imprimir Comprovantes agora }

  Result := ( Retorno in [0,1] ) ;

  if Result then
     ProcessarRespostaPagamento( IndiceFPG_ECF, Valor )
  else
     FinalizarRequisicao;
end;

function TACBrTEFDVeSPague.CHQ(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; CMC7: String; TipoPessoa: AnsiChar;
  DocumentoPessoa: String; DataCheque: TDateTime; Banco: String;
  Agencia: String; AgenciaDC: String; Conta: String; ContaDC: String;
  Cheque: String; ChequeDC: String; Compensacao: String): Boolean;
var
  Retorno : Integer;
begin
  VerificarTransacaoPagamento( Valor );

 // Até o momento não existe TAGs de transacao_ para informar os dados do CHEQUE

  Retorno := FazerRequisicao( fTransacaoCHQ, 'CHQ', Valor, DocumentoVinculado ) ;

  if Retorno = 0 then
     Retorno := ContinuarRequisicao( False ) ;  { False = NAO Imprimir Comprovantes agora }

  Result := ( Retorno in [0,1] ) ;

  if Result then
     ProcessarRespostaPagamento( IndiceFPG_ECF, Valor )
  else
     FinalizarRequisicao;
end;

procedure TACBrTEFDVeSPague.CNF(Rede, NSU, Finalizacao: String;
  DocumentoVinculado: String);
var
  P, Seq : Integer ;
  Transacao : String ;
begin
  if Finalizacao <> '' then
  begin
    P := pos('|', Finalizacao) ;
    Transacao := copy(Finalizacao,1,P-1) ;
    Seq       := StrToIntDef( copy(Finalizacao, P+1, Length(Finalizacao)), 0 );

    ReqVS.Servico := 'executar' ;
    ReqVS.AddParamString( 'transacao', Transacao ) ;
    ReqVS.Retorno    := 0 ;
    ReqVS.Sequencial := Seq;

    TransmiteCmd;
  end ;
  if (TACBrTEFD(Owner).RespostasPendentes.Count = 1) or
     ((TACBrTEFD(Owner).RespostasPendentes.Count > 0) and
       (TACBrTEFD(Owner).RespostasPendentes[TACBrTEFD(Owner).RespostasPendentes.Count -1].NSU = NSU)) then
  begin
    FinalizarRequisicao;
  end;
end;

function TACBrTEFDVeSPague.CNC(Rede, NSU: String; DataHoraTransacao: TDateTime;
  Valor: Double; CodigoAutorizacaoTransacao: String): Boolean;
var
   Retorno : Integer;
   ListaParams : AnsiString ;
begin
  ListaParams := '' ;
  if NSU <> '' then
     ListaParams := 'transacao_nsu="'+Trim(NSU)+'"';

  Retorno := FazerRequisicao( fTransacaoCNC, 'CNC', Valor, '', ListaParams  ) ;

  if Retorno = 0 then
     Retorno := ContinuarRequisicao( True ) ;  { True = Imprimir Comprovantes agora }

  Result := ( Retorno in [0,1] ) ;

  ProcessarResposta ;         { Faz a Impressão e / ou exibe Mensagem ao Operador }
end;

procedure TACBrTEFDVeSPague.NCN(Rede, NSU, Finalizacao: String; Valor: Double;
  DocumentoVinculado: String);
var
  P, Seq : Integer ;
  Transacao : String ;
begin
  if Finalizacao <> '' then
   begin
     P := pos('|', Finalizacao) ;
     Transacao := copy(Finalizacao,1,P-1) ;
     Seq       := StrToIntDef( copy(Finalizacao, P+1, Length(Finalizacao)), 0 );

     ReqVS.Servico := 'executar' ;
     ReqVS.AddParamString( 'transacao', Transacao ) ;
     ReqVS.Retorno    := 9 ;
     ReqVS.Sequencial := Seq ;

     try
        TransmiteCmd;
        FinalizarRequisicao;
     except
        if (RespVS.Retorno = 5) then   // Não achou a ultima
           ExecutarTranscaoPendente( NSU, Valor )
        else
           raise ;
     end ;
   end
  else
     FinalizarRequisicao;
end;

procedure TACBrTEFDVeSPague.ExecutarTranscaoPendente(NSU: String; Valor: Double
  );
var
   Retorno : Integer ;
   ListaParams : AnsiString ;
begin
  ListaParams := '' ;
  if NSU <> '' then
     ListaParams := 'transacao_nsu="'+Trim(NSU)+'"';

  if fTemPendencias then
    TransacaoOpcao := 'Confirmar';

  Retorno := FazerRequisicao( fTransacaoPendente, 'ADM', Valor, '', ListaParams  ) ;

  if Retorno = 0 then
     ContinuarRequisicao( True ) ;  { True = Imprimir Comprovantes agora }

  if fTemPendencias then
  begin
    fTemPendencias := False;
    FinalizarTranscaoPendente;
  end
  else
    ProcessarResposta ;         { Faz a Impressão e / ou exibe Mensagem ao Operador }
end;

procedure TACBrTEFDVeSPague.ServicoIniciar ;
begin
  repeat
     ReqVS.Servico := 'iniciar';
     ReqVS.AddParamString('aplicacao', fAplicacao);
     ReqVS.AddParamString('versao', fAplicacaoVersao);
     ReqVS.AddParamString('aplicacao_tela', '** Prioriza TEF **');
     //ReqVS.AddParamString('computador_nome', fComputadorNome);
     //ReqVS.AddParamString('computador_endereco', fComputadorEndereco);
     //ReqVS.AddParamString('estabelecimento', fEstabelecimento);
     //ReqVS.AddParamString('loja', fLoja);
     ReqVS.AddParamString('terminal', fTerminal);
     //ReqVS.AddParamString('estado', '7');

     TransmiteCmd;

     if RespVS.Sequencial < ReqVS.Sequencial then
        ReqVS.Sequencial := RespVS.Sequencial;

  until (RespVS.Retorno = 1) and (RespVS.Servico = ReqVS.Servico) ;
end ;

procedure TACBrTEFDVeSPague.ServicoFinalizar ;
begin
  ReqVS.Servico := 'finalizar' ;

  TransmiteCmd;

  if RespVS.Sequencial < ReqVS.Sequencial then
     ReqVS.Sequencial := RespVS.Sequencial;
end ;

function TACBrTEFDVeSPague.FazerRequisicao(Transacao: String;
  AHeader: AnsiString; Valor: Double; Documento: AnsiString;
  ListaParams: AnsiString): Integer;
begin
   if fpAguardandoResposta then
      raise EACBrTEFDErro.Create( ACBrStr( 'Requisição anterior não concluida' ) ) ;

   fCancelandoTransacao := (AnsiUpperCase(Transacao) = 'ADMINISTRACAO CANCELAR');

   ServicoIniciar ;

   ReqVS.Servico := 'executar' ;
   ReqVS.AddParamString( 'transacao', Transacao ) ;

   if Valor > 0 then
      ReqVS.AddParamDouble('transacao_valor', Valor);

   // V&SPague não coleta o Documento

   if TransacaoOpcao <> '' then
      ReqVS.AddParamString( 'transacao_opcao', TransacaoOpcao ) ;

   //---Adicionando o parametro CIELO PREMIA--------------
   if Transacao = fTransacaoCRT then
     ReqVS.AddParamDouble( 'transacao_valor_ajuste',  0) ;

   //-----------------------------------------------------

   if ListaParams <> '' then
      ReqVS.Params.Add(ListaParams);

   TransmiteCmd ;

   Result := RespVS.Retorno;

   Resp.Clear;

   if not (Result in [0,1]) then
      exit ;

   with TACBrTEFDRespVeSPague( Resp ) do
   begin
     if Documento = '' then
        Documento := IntToStr(fpIDSeq) ;

     { Adiciona Campos já conhecidos em Resp, para processa-los em
       métodos que manipulam "RespostasPendentes" (usa códigos do G.P.)  }
     Conteudo.GravaInformacao(899,100, AHeader ) ;
     Conteudo.GravaInformacao(899,101, IntToStr(ReqVS.Sequencial) ) ;
     Conteudo.GravaInformacao(899,102, Documento ) ;
     Conteudo.GravaInformacao(899,103, IntToStr(Trunc(SimpleRoundTo( Valor * 100 ,0))) );

     // Grava valor de "Transacao|Sequencia" em 27(Finalizacao) para usar no CNF, NCN
     Conteudo.GravaInformacao(27,0, Transacao+'|'+IntToStr(ReqVS.Sequencial) );
     Resp.TipoGP := fpTipo;
   end;

   Req.Clear;
   Req.Header := AHeader;
   Req.ID     := ReqVS.Sequencial;
   Req.DocumentoVinculado := Documento;
   Req.ValorTotal := Valor;
end;

function TACBrTEFDVeSPague.ContinuarRequisicao(ImprimirComprovantes: Boolean
  ): Integer;
var
  Chave, Valor : AnsiString ;
  I : Integer ;
begin
  fpAguardandoResposta := True ;

  with TACBrTEFD(Owner) do
  begin
     try
        BloquearMouseTeclado( True );

        Result := RespVS.Retorno;

        if RespVS.IsColeta then
        begin
           ProcessarColeta ;  // Faz Loop da coleta de Dados
           Result := RespVS.Retorno;
        end ;

        // Se Retorno = 0, o V&SPague retornou os dados do Comprovante //
        if Result in [0,1] then
        begin
           // modifica o numero da Resp.ID para o Valor retornado
           Self.Resp.Conteudo.GravaInformacao(899,101, IntToStr(RespVS.Sequencial) ) ;

           // Salvando dados do comprovante em ACBrTEFD.Resp //
           For I := 0 to RespVS.Params.Count-1 do
           begin
              Chave := RespVS.Params.Names[I] ;
              if (Chave <> '') and (pos(Chave, 'retorno,sequencial,servico') = 0) then
              begin
                 {$IFDEF COMPILER7_UP}
                  Valor := RespVS.Params.ValueFromIndex[I];
                 {$ELSE}
                  Valor := RespVS.Params.Values[Chave];
                 {$ENDIF}
                 TACBrTEFDRespVeSPague( Self.Resp ).GravaInformacao( Chave, Valor ) ;
              end ;
           end ;

           // Tem comprovante neste Retorno, e Nao precisa de Confirmacao ? //
           if (pos('transacao_comprovante_', RespVS.Params.Text ) > 0) and
              (Result = 1)  then
           begin
             // Então apague conteudo de "Finalizacao" para que CNF não funcione
             Self.Resp.Conteudo.GravaInformacao(27,0, '');
           end ;

           { Transfere valores de "Conteudo" para as propriedades }
           TACBrTEFDRespVeSPague( Self.Resp ).ConteudoToProperty ;
        end ;
     finally
        BloquearMouseTeclado( False );
        fpAguardandoResposta := False ;
     end ;
  end ;
end ;

procedure TACBrTEFDVeSPague.FinalizarRequisicao ;
var
  Mensagem : String ;
begin
  if (RespVS.Retorno = 0) and RespVS.IsColeta then
     ProcessarColeta;

  if RespVS.IsColeta then
     Mensagem := RespVS.GetParamString('automacao_coleta_mensagem')
  else
     Mensagem := RespVS.GetParamString('mensagem');

  repeat
    try
      ServicoFinalizar;
    except
    end ;
  until (RespVS.Retorno = 1) ;

  if Mensagem <> '' then
     TACBrTEFD(Owner).DoExibeMsg( opmOK, Mensagem ) ;
end ;

procedure TACBrTEFDVeSPague.FinalizarTranscaoPendente;
var
  Mensagem : String ;
  Retorno : Integer;
begin
  Retorno := RespVS.Retorno;

  if (RespVS.Retorno = 0) and RespVS.IsColeta then
     ProcessarColeta;

  if RespVS.IsColeta then
     Mensagem := RespVS.GetParamString('automacao_coleta_mensagem')
  else
     Mensagem := RespVS.GetParamString('mensagem');

  repeat
    try
      ServicoFinalizar;
    except
    end ;
  until (RespVS.Retorno = 1) ;

  if Mensagem <> '' then
     TACBrTEFD(Owner).DoExibeMsg( opmOK, Mensagem ) ;

  if Retorno = 9 then
    raise EACBrTEFDErro.Create('A operação de desfazimento será reiniciada.');
end;

procedure TACBrTEFDVeSPague.ProcessarColeta ;
var
  Mensagem, Mascara, Msg : AnsiString ;
  Resposta : String ;
  Tipo : AnsiChar ;
  OpcoesMenu, Video_Mensagem : TStringList ;
  ItemSelecionado, OldSeq : Integer ;
  Cancelar, Digitado : Boolean ;
  MR : TModalResult ;
  MostraMsgDebito, MostraMsgCredito, MostrouMsgConfirmacao: Boolean;
  Respostas: TStringList;
begin
  with TACBrTEFD(Owner) do
  begin
     OldSeq         := ReqVS.Sequencial;
     OpcoesMenu     := TStringList.Create;
     Video_Mensagem := TStringList.Create;
     Respostas      := TStringList.Create;
     MostrouMsgConfirmacao   := False;

     try
        while RespVS.IsColeta and (RespVS.Retorno = 0) do
        begin
           Cancelar := False ;
           Resposta := '';
           Mensagem := RespVS.GetParamString('automacao_coleta_mensagem');
           Mascara  := RespVS.GetParamString('automacao_coleta_mascara');
           Tipo     := AnsiChar(PadRight(RespVS.GetParamString('automacao_coleta_tipo'),1)[1]) ;
           RespVS.GetParamStrings('automacao_coleta_opcao', OpcoesMenu);
           Video_Mensagem.Text := RespVS.GetParamString('automacao_coleta_video_mensagem');

           MostraMsgDebito := fCancelandoTransacao and
                          (AnsiContainsText(AnsiUpperCase(Mensagem), 'SOLICITANDO'));
           MostraMsgCredito := fCancelandoTransacao and
                          (AnsiUpperCase(Mensagem) = 'VALOR CANCELAMENTO');

           if MostraMsgDebito and not MostrouMsgConfirmacao then // transacao de débito
           begin
             MostrouMsgConfirmacao := True;
             Msg := 'Data da Venda: '+ Respostas.Values['3'] +sLineBreak+
                    'Valor da Venda: '+ Respostas.Values['5'] +SLineBreak+
                    //'Valor a Cancelar: '+ Respostas.Values['7'] +SLineBreak+
                    'Docto: '+ Respostas.Values['4'] +SLineBreak+SLineBreak+
                    'Confirma o cancelamento desta transação?';

             if DoExibeMsg( opmYesNo, Msg) = mrNo then
             begin
               Mensagem := '';
               Cancelar := True;
             end 
             else
               fpSalvarArquivoBackup := False;
           end;

           if OpcoesMenu.Count > 0 then            // Tem Menu ?
           begin
              if OpcoesMenu.Count = 1 then
              begin
                 Resposta := OpcoesMenu[0]
              end
              else if (OpcoesMenu.Count = 2) and (OpcoesMenu[0] = 'Sim') and (OpcoesMenu[1] = ACBrStr('Não') ) then
              begin
                 MR := DoExibeMsg( opmYesNo, Mensagem ) ;

                 if MR = mrYes then
                    Resposta := OpcoesMenu[0]
                 else if MR = mrNo then
                    Resposta := OpcoesMenu[1]
                 else
                    Cancelar := True ;
              end
              else
              begin
                 ItemSelecionado := -1 ;
                 BloquearMouseTeclado(False);
                 OnExibeMenu( ACBrStr(Mensagem), OpcoesMenu, Video_Mensagem, ItemSelecionado ) ;
                 BloquearMouseTeclado(True);

                 if (ItemSelecionado >= 0) and (ItemSelecionado < OpcoesMenu.Count) then
                    Resposta := OpcoesMenu[ ItemSelecionado ]
                 else
                    Cancelar := True ;
              end ;
           end
           else if Tipo <> ' ' then                // Tem Pergunta ?
           begin
             BloquearMouseTeclado(False);
             Digitado := False ;
             OnObtemCampo( ACBrStr(Mensagem), Mascara, Tipo, Resposta, Digitado) ;
             BloquearMouseTeclado(True);

             Cancelar := not Digitado;
           end
           else if Mensagem <> '' then             // Tem Mensagem ?
           begin
             DoExibeMsg( opmExibirMsgOperador, Mensagem ) ;
           end ;

           // Respondendo a Coleta //
           ReqVS.Clear;
           ReqVS.IsColeta   := True ;
           ReqVS.Sequencial := RespVS.Sequencial;

           if Resposta <> '' then
           begin
             if Tipo = 'N' then
               ReqVS.AddParamDouble( 'automacao_coleta_informacao', StrToFloatDef(Resposta, 0))
             else
               ReqVS.AddParamString( 'automacao_coleta_informacao', Resposta ) ;

             Respostas.Add(Format('%d=%s', [ReqVS.Sequencial, Resposta]));
           end;

           if MostraMsgCredito and not MostrouMsgConfirmacao then // transacao de credito
           begin
             MostrouMsgConfirmacao := True;
             Msg := 'Data da Venda: '+ Respostas.Values['3'] +sLineBreak+
                    'Valor da Venda: '+ Respostas.Values['5'] +SLineBreak+
                    'Valor a Cancelar: '+ Respostas.Values['7'] +SLineBreak+
                    'Docto: '+ Respostas.Values['4'] +SLineBreak+SLineBreak+
                    'Confirma o cancelamento desta transação?';

             if DoExibeMsg( opmYesNo, Msg) = mrNo then
               Cancelar := True
             else
               fpSalvarArquivoBackup := False;
           end;

           ReqVS.Retorno := 0; //ReqVS.Retorno := ifthen(Cancelar, 9, 0);

           TransmiteCmd;
        end ;
     finally
        ReqVS.Clear;
        ReqVS.IsColeta    := False ;
        ReqVS.Sequencial  := OldSeq ;

        OpcoesMenu.Free;
        Video_Mensagem.Free;
        Respostas.Free;
     end ;
  end ;
end ;

procedure TACBrTEFDVeSPague.TransmiteCmd;
Var
  RX : AnsiString ;
  Erro, Seq, Retorno : Integer ;
  Interromper : Boolean ;

  procedure EnviarCmd ;
  var
    TX : AnsiString ;
  begin
    // Enviado comando //
    TX := ReqVS.FrameEnvio + sLineBreak +
          '		' + sLineBreak +
          '			' + sLineBreak +
          '		' + sLineBreak +
          '	';
    GravaLog( 'TRANSMITINDO ->'+sLineBreak+TX );
    fSocket.SendString(TX);
    Erro := fSocket.LastError ;
    GravaLog( '  TRANSMITIDO, ('+IntToStr(Erro)+') '+fSocket.GetErrorDesc(Erro)+sLineBreak );
    if Erro <> 0 then
       raise EACBrTEFDErro.Create( ACBrStr('Erro ao Transmitir Comando para V&SPague'+sLineBreak+
                               'Endereço: '+fEnderecoIP+sLineBreak+
                               'Porta: '+fPorta+sLineBreak+
                               'Erro: '+IntToStr(Erro)+'-'+fSocket.GetErrorDesc(Erro) ) ) ;
  end;

begin
  if not Assigned(fSocket) then
     raise EACBrTEFDErro.Create( ACBrStr('TEF '+Name+' não inicializado') );

  RespVS.Clear;    // Limpa a resposta

  try
     EnviarCmd ;   // Transmite CMD
  except
     // 10054-Connection reset by peer; 10057-Socket is not connected
     Erro := fSocket.LastError ;
     if (Erro = 10054) or (Erro = 10057) then
      begin
        GravaLog( '** Tentando Re-conexao' );

        DesConectar;
        Sleep(1000);
        Conectar;

        EnviarCmd;   // Re-Transmite CMD
      end
     else
        raise ;
  end ;

  // Faz Loop por Espera de Resposta //
  // em operações que dependem do PinPad, pode haver vários TimeOuts //
  Erro := -1 ;
  while (Erro <> 0) do
  begin
     // Aguardando a Resposta //
     GravaLog( 'Aguardando Resposta do V&SPague' );
     RX := fSocket.RecvTerminated(fTimeOut,fTerminador);
     Erro := fSocket.LastError ;

     if Erro = 0 then
      begin
        GravaLog( '<- RECEBIDO' + sLineBreak + RX ) ;

        // Trata a Resposta //
        RespVS.FrameEnvio := RX ;

        Retorno := RespVS.Retorno ;
        if Retorno = 2 then                // Erro := 'Sequencial Inválido' ;
        begin
           ReqVS.Sequencial := RespVS.Sequencial;  // Ajusta o Sequencial correto
           EnviarCmd;
           Erro := 2 ;                             // Tenta novamente
           Continue;
        end ;

        AvaliaErro(Retorno) ;              // Dispara Exception se houver erro
      end
     else
      begin
        GravaLog( '<- ERRO, ('+IntToStr(Erro)+') '+fSocket.GetErrorDesc(Erro) );

        if Erro = 10060 then   // Ocorreu TimeOut (10060) ?
         begin
           // Chama Evento para dar chance do usuário cancelar //
           Interromper := False ;
           TACBrTEFD(Owner).OnAguardaResp( '23', 0, Interromper ) ;
                                           // 23 = Compatibilidade com CliSiTEF

           if Interromper then          // Usuário Cancelou ?
           begin
              GravaLog('** Interrompido pelo usuário **');

              Seq := ReqVS.Sequencial;  // Salva o ultimo sequencial

              // Envia novo comando com Retorno=9 para cancelar //
              ReqVS.Clear;
              ReqVS.IsColeta   := True ;
              ReqVS.Sequencial := Seq + 1;
              ReqVS.Retorno    := 9 ;

              EnviarCmd;
              Erro := -1;
              Continue;
           end ;
         end
        else
           raise EACBrTEFDErro.Create( ACBrStr('Erro ao Receber resposta do V&SPague'+sLineBreak+
                        'Endereço: '+fEnderecoIP+sLineBreak+
                        'Porta: '+fPorta+sLineBreak+
                        'Erro: '+IntToStr(Erro)+'-'+fSocket.GetErrorDesc(Erro) ) ) ;
     end ;
  end ;

end ;

procedure TACBrTEFDVeSPague.AvaliaErro( Retorno : Integer) ;
var
   Erro : String;
begin
  // 0, 1 : Tudo Ok
  // 9   : Interrompido, deve ser tratado pela função chamadora, (chame ExibirMensagens)
  if Retorno in [0,1,3,9] then
     exit ;

  if RespVS.IsColeta then
     Erro := RespVS.GetParamString('automacao_coleta_mensagem')
  else
     Erro := RespVS.GetParamString('mensagem');

  if Erro = '' then
  begin
    Case Retorno of
      2 : Erro := 'Sequencial Inválido' ;
      // 3, 4 : Não são utilizados
      5 : Erro := 'Parâmetros insuficientes ou inválidos' ;
      // 6, 7 : Não são utilizados
      8 : Erro := 'Tempo limite de espera excedido' ;
    end ;
  end ;

  if Erro <> '' then
     raise EACBrTEFDSTSInvalido.Create( ACBrStr(Erro) );

{  if RespVS.Sequencial <> ReqVS.Sequencial then
     raise EACBrTEFDSTSInvalido.Create( ACBrStr('Sequencia da Resposta inválida') );}
end ;


procedure TACBrTEFDVeSPague.ProcessarResposta ;
var
   RespostaPendente: TACBrTEFDRespVeSPague;
begin
  VerificarIniciouRequisicao;

  with TACBrTEFD(Owner) do
  begin
     GravaLog( Name +' ProcessarResposta: '+Req.Header );

     EstadoResp := respProcessando;

     if Resp.QtdLinhasComprovante > 0 then
     begin
       { Cria cópia do Objeto Resp, e salva no ObjectList "RespostasPendentes" }
       RespostaPendente := TACBrTEFDRespVeSPague.Create ;
       try
         RespostaPendente.Assign( Resp );
         RespostasPendentes.Add( RespostaPendente );

         if fCancelandoTransacao then
           CNF(Resp.Rede, Resp.NSU, Resp.Finalizacao, Resp.DocumentoVinculado);

         try
           ImprimirRelatorio ;
         finally
           fpSalvarArquivoBackup := True;
         end;

         if Assigned( OnDepoisConfirmarTransacoes ) then
           OnDepoisConfirmarTransacoes( RespostasPendentes );
       finally
         RespostasPendentes.Clear;
       end;
     end
     else
     begin
       if Resp.TextoEspecialOperador <> '' then
         DoExibeMsg( opmOK, Resp.TextoEspecialOperador ) ;
       FinalizarRequisicao;
     end ;
  end ;
end;

function TACBrTEFDVeSPague.ProcessarRespostaPagamento(
  const IndiceFPG_ECF: String; const Valor: Double): Boolean;
var
  ImpressaoOk : Boolean;
  RespostaPendente : TACBrTEFDResp ;
begin
  Result := True ;

  with TACBrTEFD(Owner) do
  begin
     Self.Resp.IndiceFPG_ECF := IndiceFPG_ECF;

     { Cria Arquivo de Backup, contendo Todas as Respostas }
     CopiarResposta ;

     { Cria cópia do Objeto Resp, e salva no ObjectList "RespostasPendentes" }
     Resp.ViaClienteReduzida := ImprimirViaClienteReduzida;
     RespostaPendente := TACBrTEFDRespVeSPague.Create ;
     RespostaPendente.Assign( Resp );
     RespostasPendentes.Add( RespostaPendente );

     if AutoEfetuarPagamento then
     begin
        ImpressaoOk := False ;

        try
           while not ImpressaoOk do
           begin
              try
                 ECFPagamento( IndiceFPG_ECF, Valor );
                 RespostasPendentes.SaldoAPagar  := RoundTo( RespostasPendentes.SaldoAPagar - Valor, -2 ) ;
                 RespostaPendente.OrdemPagamento := RespostasPendentes.Count + 1 ;
                 ImpressaoOk := True ;
              except
                 on EACBrTEFDECF do ImpressaoOk := False ;
                 else
                    raise ;
              end;

              if not ImpressaoOk then
              begin
                 if DoExibeMsg( opmYesNo, 'Impressora não responde'+sLineBreak+
                                          'Deseja imprimir novamente ?') <> mrYes then
                 begin
                    try ComandarECF(opeCancelaCupom); except {Exceção Muda} end ;
                    break ;
                 end;
              end;
           end;
        finally
           if not ImpressaoOk then
              CancelarTransacoesPendentes;
        end;
     end;

     if RespostasPendentes.SaldoRestante <= 0 then
     begin
        if AutoFinalizarCupom then
        begin
           FinalizarCupom( False );  { False não desbloqueia o MouseTeclado }
           ImprimirTransacoesPendentes;
        end;
     end ;
  end;
end;

(*
DUVIDAS:

- O que deve ser feito após receber Transacao NEGADA ??

      -- 05/07/10 13:02:05
      Aguardando Resposta do V&SPague
      -- 05/07/10 13:02:05
      <- RECEBIDO
      automacao_coleta_retorno="9"
      automacao_coleta_sequencial="8"
      automacao_coleta_mensagem="[00088]: Transação negada pela rede."

      -- 05/07/10 13:02:05
      BloquearMouseTeclado: NAO
      -- 05/07/10 13:02:21
      TRANSMITINDO ->
      sequencial="62"
      retorno="1"
      servico="finalizar"

      -- 05/07/10 13:02:21
        TRANSMITIDO, (0)

      -- 05/07/10 13:02:21
      Aguardando Resposta do V&SPague
      -- 05/07/10 13:02:21
      <- RECEBIDO
      sequencial="61"
      retorno="3"
      mensagem="Transacao cancelada pelo operador."

-  Se a transação Falhar, com o Cartão inserido, V&SPague para de Responder...

      -- 06/07/10 19:06:14
      Aguardando Resposta do V&SPague
      -- 06/07/10 19:06:14
      <- RECEBIDO
      automacao_coleta_retorno="0"
      automacao_coleta_sequencial="1"
      automacao_coleta_mensagem="RETIRE O CARTÃO"

      -- 06/07/10 19:06:14
      VeSPague DoExibeMsg: Oper: opmExibirMsgOperador Mensagem: RETIRE O CARTÃO
      -- 06/07/10 19:06:14
      TRANSMITINDO ->
      automacao_coleta_sequencial="1"
      automacao_coleta_retorno="0"

      -- 06/07/10 19:06:14
        TRANSMITIDO, (0)

      -- 06/07/10 19:06:14
      Aguardando Resposta do V&SPague
      -- 06/07/10 19:06:15
      <- ERRO, (10060) Connection timed out
      -- 06/07/10 19:06:15
      Aguardando Resposta do V&SPague
      -- 06/07/10 19:06:15
      <- ERRO, (10060) Connection timed out
      -- 06/07/10 19:06:15


- Chamar Administraçao Pendente, sem ter algo pendente TRAVA o Client

- No processo de Coleta, o VSCliente não envia mensagem informado o Operador
  para digitar a Senha no Pin-Pad

- VSCliente não envia mensagem para limpeza da Mensagem do Operador

*)
end.


