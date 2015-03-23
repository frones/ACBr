{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

{.$IFDEF FPC}
 {$DEFINE Use_Stream}
{.$ENDIF}

unit ACBrECFVirtualPrinter ;

interface
uses ACBrECFVirtual, ACBrECFClass, ACBrDevice, ACBrUtil, ACBrConsts,
     Classes, SysUtils, IniFiles;

const
  cCmdImpCondensado        = #15 ;
  cCmdImpExpandidoUmaLinha = #14 ;
  cCmdImpFimExpandido      = #20;
  cCmdImpZera              = #27+'@' ;
  cCmdGaveta               = #27+'v'+#150;
  cCmdCortaPapelCompleto   = #27+#119;
  cCmdCortaPapelParcial    = #27+#109;
  ACBrECFVirtualPrinter_VERSAO = '0.1.0a';


type

{ TACBrECFVirtualPrinter }

TACBrECFVirtualPrinter = class( TACBrECFVirtual )
  private
    function GetCabecalho: TStrings;
    function GetCabecalhoItem: TStrings;
    function GetMascaraItem: String;
    function GetCmdCortaPapelCompleto: AnsiString;
    function GetCmdCortaPapelParcial: AnsiString;
    function GetCmdGaveta: AnsiString;
    function GetCmdImpCondensado: AnsiString;
    function GetCmdImpExpandidoUmaLinha: AnsiString;
    function GetCmdImpFimExpandido: AnsiString;
    function GetCmdImpZera: AnsiString;
    procedure SetCabecalho(AValue: TStrings);
    procedure SetCabecalhoItem(AValue: TStrings);
    procedure SetMascaraItem(AValue: String);
    procedure SetCmdCortaPapelCompleto(AValue: AnsiString);
    procedure SetCmdCortaPapelParcial(AValue: AnsiString);
    procedure SetCmdGaveta(AValue: AnsiString);
    procedure SetCmdImpCondensado(AValue: AnsiString);
    procedure SetCmdImpExpandidoUmaLinha(AValue: AnsiString);
    procedure SetCmdImpFimExpandido(AValue: AnsiString);
    procedure SetCmdImpZera(AValue: AnsiString);
  protected
    procedure CreateVirtualClass ; override ;

    property CmdImpCondensado : AnsiString read GetCmdImpCondensado
       write SetCmdImpCondensado ;
    property CmdImpExpandidoUmaLinha : AnsiString read GetCmdImpExpandidoUmaLinha
       write SetCmdImpExpandidoUmaLinha ;
    property CmdImpFimExpandido : AnsiString read GetCmdImpFimExpandido
       write SetCmdImpFimExpandido ;
    property CmdImpZera : AnsiString read GetCmdImpZera
       write SetCmdImpZera ;
    property CmdGaveta  : AnsiString read GetCmdGaveta
       write SetCmdGaveta ;
    property CmdCortaPapelCompleto : AnsiString read GetCmdCortaPapelCompleto
       write SetCmdCortaPapelCompleto ;
    property CmdCortaPapelParcial : AnsiString read GetCmdCortaPapelParcial
       write SetCmdCortaPapelParcial ;

    property Cabecalho     : TStrings read GetCabecalho     write SetCabecalho;
    property CabecalhoItem : TStrings read GetCabecalhoItem write SetCabecalhoItem;
    property MascaraItem   : String   read GetMascaraItem   write SetMascaraItem;
  end ;

{ TACBrECFVirtualPrinterClass }

TACBrECFVirtualPrinterClass = class( TACBrECFVirtualClass )
  private
    {$IFDEF Use_Stream}
      fsFSBuffer : TFileStream ;
    {$ELSE}
      fsArqBuf : TextFile;
    {$ENDIF}
    fsBuffer : TStringList ;

    fsCabecalho : TStringList ;
    fsCabecalhoItem : TStringList ;
    fsMascaraItem : String ;

    fsCmdImpCondensado: AnsiString;
    fsCmdImpExpandidoUmaLinha: AnsiString;
    fsCmdImpFimExpandido: AnsiString;
    fsCmdImpZera: AnsiString;
    fsCmdGaveta: AnsiString;
    fsCmdCortaPapelCompleto: AnsiString;
    fsCmdCortaPapelParcial: AnsiString;

    procedure AbreBuffer ;
    procedure GravaBuffer ;
    procedure SetCabecalho(AValue: TStringList);
    procedure SetCabecalhoItem(AValue: TStringList);
    procedure ZeraBuffer ;
    procedure ImprimeBuffer ;

    procedure InsertBufferCabecalho;
    procedure AddBufferCabecalho_Item;
    procedure AddBufferRelatorio ;
    procedure AddBufferRodape;
    procedure AddBufferLinhas( AString: AnsiString) ;

    function ColunasExpandido: Integer;
    function AjustaLinhaColunas( Linha: AnsiString ): AnsiString;

  protected
    procedure AtivarVirtual ; override;

    procedure AbreDocumento ; override;
    procedure AbreDocumentoVirtual ; override;
    Procedure EnviaConsumidorVirtual ; override;
    procedure VendeItemVirtual( ItemCupom: TACBrECFVirtualClassItemCupom); override;
    Procedure CancelaItemVendidoVirtual( NumItem : Integer ) ; override ;
    Procedure SubtotalizaCupomVirtual( DescontoAcrescimo : Double = 0;
       MensagemRodape : AnsiString  = '' ) ; override ;
    Procedure EfetuaPagamentoVirtual( Pagto: TACBrECFVirtualClassPagamentoCupom) ; override ;
    Procedure FechaCupomVirtual( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaCupomVirtual ; override ;

    Procedure LeituraXVirtual ; override ;
    Procedure ReducaoZVirtual(DataHora : TDateTime = 0 ) ; override ;

    procedure AbreNaoFiscalVirtual(CPF_CNPJ: String; Nome: String; Endereco: String
      ); override;
    Procedure RegistraItemNaoFiscalVirtual( CNF : TACBrECFComprovanteNaoFiscal;
       Valor : Double; Obs : AnsiString = '') ; override ;
    procedure AbreRelatorioGerencialVirtual(Indice: Integer); override;
    Procedure AbreCupomVinculadoVirtual(COO: String; FPG: TACBrECFFormaPagamento;
       CodComprovanteNaoFiscal : String; Valor : Double) ; override ;
    procedure FechaRelatorioVirtual; override;


  protected
    procedure ImprimePorta( AString : AnsiString ) ; overload ;
    procedure ImprimePorta( AStringList : TStringList ) ; overload ;

    function GetSubModeloECF: String ; override ;
    function GetNumVersao: String; override ;
    function GetCliche: AnsiString ; override ;

  public
    Constructor create( AOwner : TComponent  )  ;
    Destructor Destroy  ; override ;

    procedure INItoClass( ConteudoINI: TStrings ) ; override;
    procedure ClasstoINI( ConteudoINI: TStrings ); override;

    property CmdImpCondensado : AnsiString read fsCmdImpCondensado
       write fsCmdImpCondensado ;
    property CmdImpExpandidoUmaLinha : AnsiString read fsCmdImpExpandidoUmaLinha
       write fsCmdImpExpandidoUmaLinha ;
    property CmdImpFimExpandido : AnsiString read fsCmdImpFimExpandido
       write fsCmdImpFimExpandido ;
    property CmdImpZera : AnsiString read fsCmdImpZera write fsCmdImpZera ;
    property CmdGaveta  : AnsiString read fsCmdGaveta  write fsCmdGaveta ;
    property CmdCortaPapelCompleto : AnsiString read fsCmdCortaPapelCompleto
       write fsCmdCortaPapelCompleto ;
    property CmdCortaPapelParcial : AnsiString read fsCmdCortaPapelParcial
       write fsCmdCortaPapelParcial ;

    property Cabecalho     : TStringList read fsCabecalho     write SetCabecalho;
    property CabecalhoItem : TStringList read fsCabecalhoItem write SetCabecalhoItem;
    property MascaraItem   : String      read fsMascaraItem   write fsMascaraItem;

    procedure Desativar ; override ;
    Function EnviaComando_ECF( cmd : AnsiString ) : AnsiString ; override ;

    Procedure AbreCupom ; override ;
    Procedure FechaCupom( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;

    Procedure LeituraX ; override ;
    Procedure ReducaoZ(DataHora : TDateTime = 0 ) ; override ;
    Procedure LinhaRelatorioGerencial( Linha : AnsiString; IndiceBMP: Integer = 0 ) ; override ;
    Procedure LinhaCupomVinculado( Linha : AnsiString ) ; override ;
    Procedure CortaPapel( const CorteParcial : Boolean = false) ; override ;

    Procedure AbreGaveta ; override ;
  end ;

Function StuffMascaraItem( Linha, MascaraItem : AnsiString; Letra : AnsiChar;
       TextoInserir : AnsiString; Fim:Boolean = False) : AnsiString ;

implementation

Uses math ;

Function StuffMascaraItem( Linha, MascaraItem : AnsiString; Letra : AnsiChar;
   TextoInserir : AnsiString; Fim:Boolean = False) : AnsiString ;
Var A,B : Integer ;
    L   : AnsiChar ;
begin
  Result := '' ;

  if not FIM then
   begin
     B := 1 ;
     For A := 1 to length(MascaraItem) do
     begin
        L := MascaraItem[A] ;

        if L = Letra then
           if B > Length( TextoInserir ) then
              L := ' '
           else
            begin
              L := TextoInserir[B] ;
              B := B + 1 ;
            end
        else
           if A > Length( Linha ) then
              L := ' '
           else
              L := Linha[A] ;

        Result := Result + L ;
     end
   end
  else
   begin
     B := length(TextoInserir) ;
     For A := length(MascaraItem) downto 1 do
     begin
        L := MascaraItem[A] ;

        if L = Letra then
           if B < 1 then
              L := ' '
           else
            begin
              L := TextoInserir[B] ;
              B := B - 1 ;
            end
        else
           if A > Length( Linha ) then
              L := ' '
           else
              L := Linha[A] ;

        Result := L + Result ;
     end
   end;
end;

{ TACBrECFVirtualPrinter }

procedure TACBrECFVirtualPrinter.CreateVirtualClass;
begin
  fpECFVirtualClass := TACBrECFVirtualPrinterClass.create( self );
end;

function TACBrECFVirtualPrinter.GetCmdCortaPapelCompleto: AnsiString;
begin
  Result := StringToAsc( TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdCortaPapelCompleto );
end;

function TACBrECFVirtualPrinter.GetCabecalho: TStrings;
begin
  Result := TACBrECFVirtualPrinterClass(fpECFVirtualClass).Cabecalho;
end;

function TACBrECFVirtualPrinter.GetCabecalhoItem: TStrings;
begin
  Result := TACBrECFVirtualPrinterClass(fpECFVirtualClass).CabecalhoItem;
end;

function TACBrECFVirtualPrinter.GetCmdCortaPapelParcial: AnsiString;
begin
  Result := StringToAsc( TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdCortaPapelParcial );
end;

function TACBrECFVirtualPrinter.GetCmdGaveta: AnsiString;
begin
  Result := StringToAsc( TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdGaveta );
end;

function TACBrECFVirtualPrinter.GetCmdImpCondensado: AnsiString;
begin
  Result := StringToAsc( TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdImpCondensado );
end;

function TACBrECFVirtualPrinter.GetCmdImpExpandidoUmaLinha: AnsiString;
begin
  Result := StringToAsc( TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdImpExpandidoUmaLinha );
end;

function TACBrECFVirtualPrinter.GetCmdImpFimExpandido: AnsiString;
begin
  Result := StringToAsc( TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdImpFimExpandido );
end;

function TACBrECFVirtualPrinter.GetCmdImpZera: AnsiString;
begin
  Result := StringToAsc( TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdImpZera );
end;

function TACBrECFVirtualPrinter.GetMascaraItem: String;
begin
  Result := TACBrECFVirtualPrinterClass(fpECFVirtualClass).MascaraItem;
end;

procedure TACBrECFVirtualPrinter.SetCabecalho(AValue: TStrings);
begin
  TACBrECFVirtualPrinterClass(fpECFVirtualClass).Cabecalho.Assign( AValue );
end;

procedure TACBrECFVirtualPrinter.SetCabecalhoItem(AValue: TStrings);
begin
  TACBrECFVirtualPrinterClass(fpECFVirtualClass).CabecalhoItem.Assign( AValue );
end;

procedure TACBrECFVirtualPrinter.SetCmdCortaPapelCompleto(AValue: AnsiString);
begin
  TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdCortaPapelCompleto := AscToString( AValue );
end;

procedure TACBrECFVirtualPrinter.SetCmdCortaPapelParcial(AValue: AnsiString);
begin
  TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdCortaPapelParcial := AscToString( AValue );
end;

procedure TACBrECFVirtualPrinter.SetCmdGaveta(AValue: AnsiString);
begin
  TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdGaveta := AscToString( AValue );
end;

procedure TACBrECFVirtualPrinter.SetCmdImpCondensado(AValue: AnsiString);
begin
  TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdImpCondensado := AscToString( AValue );
end;

procedure TACBrECFVirtualPrinter.SetCmdImpExpandidoUmaLinha(AValue: AnsiString);
begin
  TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdImpExpandidoUmaLinha := AscToString( AValue );
end;

procedure TACBrECFVirtualPrinter.SetCmdImpFimExpandido(AValue: AnsiString);
begin
  TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdImpFimExpandido := AscToString( AValue );
end;

procedure TACBrECFVirtualPrinter.SetCmdImpZera(AValue: AnsiString);
begin
  TACBrECFVirtualPrinterClass(fpECFVirtualClass).CmdImpZera := AscToString( AValue );
end;

procedure TACBrECFVirtualPrinter.SetMascaraItem(AValue: String);
begin
  TACBrECFVirtualPrinterClass(fpECFVirtualClass).MascaraItem := AValue;
end;

{ TACBrECFVirtualPrinterClass }

constructor TACBrECFVirtualPrinterClass.create( AOwner : TComponent ) ;
begin
  inherited create( AOwner ) ;

  fsCmdImpCondensado        := cCmdImpCondensado ;
  fsCmdImpExpandidoUmaLinha := cCmdImpExpandidoUmaLinha ;
  fsCmdImpFimExpandido      := cCmdImpFimExpandido;
  fsCmdImpZera              := cCmdImpZera ;
  fsCmdGaveta               := cCmdGaveta;
  fsCmdCortaPapelCompleto   := cCmdCortaPapelCompleto;
  fsCmdCortaPapelParcial    := cCmdCortaPapelParcial;

  fsBuffer := TStringList.create ;

  fsCabecalho := TStringList.create ;
  fsCabecalho.add('Nome da Empresa') ;
  fsCabecalho.add('Nome da Rua , 1234  -  Bairro') ;
  fsCabecalho.add('Cidade  -  UF  -  99999-999') ;

  fsCabecalhoItem := TStringList.create ;
  fsCabecalhoItem.Add('ITEM   CODIGO             DESCRICAO') ;
  fsCabecalhoItem.Add('.             QTDxUNITARIO   Aliq    VALOR (R$)') ;
  fsCabecalhoItem.Add( StringOfChar('-',Colunas) ) ;

  fsMascaraItem := 'III CCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD '+
                   'QQQQQQQQ UUxVVVVVVVVV AAAAAAA TTTTTTTTTTT' ;
end;

destructor TACBrECFVirtualPrinterClass.Destroy;
begin
  fsBuffer.Free ;
  fsCabecalho.Free ;
  fsCabecalhoItem.Free ;

  Desativar ;

  inherited Destroy ;
end;

procedure TACBrECFVirtualPrinterClass.AtivarVirtual;
begin
  if fpDevice.IsSerialPort then
    fpDevice.Serial.Purge ;

  if not EmLinha() then
    raise EACBrECFERRO.Create(ACBrStr('Impressora: '+fpModeloStr+' não está pronta.')) ;

  AbreBuffer ;

  ImprimePorta( CmdImpZera + CmdImpCondensado );

  if fsBuffer.Count > 0 then
    ImprimeBuffer ;
end;

procedure TACBrECFVirtualPrinterClass.Desativar;
begin
  {$IFDEF Use_Stream}
    if Assigned( fsFSBuffer ) then
      FreeAndNil( fsFSBuffer );
  {$ELSE}
    {$I-}
    try
      CloseFile( fsArqBuf ) ;
    except
    end ;
    {$I+}
  {$ENDIF}

  inherited Desativar ;
end;

function TACBrECFVirtualPrinterClass.EnviaComando_ECF(cmd : AnsiString
  ) : AnsiString ;
begin
  cmd := AjustaLinhaColunas(cmd) ;
  ImprimePorta( cmd );

  fpComandoEnviado := cmd ;
  Result           := cmd ;
end;

function TACBrECFVirtualPrinterClass.GetSubModeloECF: String;
begin
  Result := 'VirtualPrinter' ;
end;

function TACBrECFVirtualPrinterClass.GetNumVersao: String ;
begin
  Result := ACBrECFVirtualPrinter_VERSAO ;
end;

function TACBrECFVirtualPrinterClass.GetCliche : AnsiString ;
Var
  A : Integer ;
begin
  Result := '' ;
  For A := 0 to fsCabecalho.Count - 1 do
    Result := Result + PadCenter(fsCabecalho[A], Colunas) + CRLF;
end ;

procedure TACBrECFVirtualPrinterClass.AbreBuffer ;
Var
  NomeArqBuffer : String ;
  Mode: Word;
begin
  NomeArqBuffer := ChangeFileExt( NomeArqINI, '.buf') ;

  {$IFDEF Use_Stream}
    if FileExists( NomeArqBuffer ) then
    begin
      fsBuffer.LoadFromFile( NomeArqBuffer );
      Mode := fmOpenReadWrite;
    end
    else
      Mode := fmCreate;

    fsFSBuffer := TFileStream.Create( NomeArqBuffer, Mode or fmShareDenyWrite );
    fsFSBuffer.Seek(0, soFromEnd);  // vai para EOF
  {$ELSE}
    AssignFile( fsArqBuf, NomeArqBuffer );
    FileMode := fmOpenReadWrite + fmShareExclusive ;

    try
      if FileExists( NomeArqBuffer ) then
      begin
        fsBuffer.LoadFromFile( NomeArqBuffer );
        Reset( fsArqBuf );
        if not SeekEof( fsArqBuf ) then
          raise EACBrECFERRO.Create(ACBrStr('Erro ao posicionar em EOF no arquivo: ')+
                                    NomeArqBuffer) ;
      end
      else
        ReWrite( fsArqBuf );
    except
      try
        {$I-}
        CloseFile( fsArqBuf );
        IOResult;
        {$I+}
      except
      end ;

      raise ;
    end ;
  {$ENDIF}
end;

procedure TACBrECFVirtualPrinterClass.GravaBuffer ;
var
  A : Integer ;
  Buffer: String;
begin
  {$IFDEF Use_Stream}
    if not Assigned( fsFSBuffer ) then
      exit;
  {$ENDIF}

  For A := 0 to fsBuffer.Count - 1 do
  begin
    Buffer := fsBuffer[A];

    {$IFDEF Use_Stream}
      Buffer := Buffer + sLineBreak;
      fsFSBuffer.Write(Pointer(Buffer)^,Length(Buffer));
    {$ELSE}
      Writeln( fsArqBuf, Buffer ) ;
    {$ENDIF}
  end ;
end ;

procedure TACBrECFVirtualPrinterClass.SetCabecalho(AValue: TStringList);
begin
  fsCabecalho.Assign( AValue );
end;

procedure TACBrECFVirtualPrinterClass.SetCabecalhoItem(AValue: TStringList);
begin
  fsCabecalhoItem.Assign( AValue );
end;

procedure TACBrECFVirtualPrinterClass.ZeraBuffer ;
begin
  {$IFDEF Use_Stream}
    if Assigned( fsFSBuffer ) then
      fsFSBuffer.Size := 0;
  {$ELSE}
    Rewrite( fsArqBuf ) ;
  {$ENDIF}
  fsBuffer.Clear ;
end ;

procedure TACBrECFVirtualPrinterClass.ImprimeBuffer ;
begin
  ImprimePorta( fsBuffer );
  ZeraBuffer ;
end ;

procedure TACBrECFVirtualPrinterClass.AddBufferRelatorio;
Var
  TotalAliq, BrutaDia : Double;
  A : Integer ;
begin
  TotalAliq := 0 ;
  BrutaDia  := 0 ;
  For A := 0 to 2 do
    with fpAliquotas[A] do
      TotalAliq := RoundTo(TotalAliq + Total,-2) ;

  with fsBuffer do
  begin
    Add( StringOfChar('-',Colunas) ) ;
    Add( PadCenter(' Contadores ',Colunas,'-') ) ;
    Add( PadSpace('Reducoes Z:|'+IntToStrZero(fpReducoesZ,4),Colunas,'|') ) ;
    Add( PadSpace('Leitura  X:|'+IntToStrZero(fpLeiturasX,6),Colunas,'|') ) ;
    Add( PadSpace('Cancelamentos de Cupom:|'+IntToStrZero(fpCuponsCancelados,6), Colunas,'|') ) ;
    Add( PadSpace('COO do Primeiro Cupom:|'+IntToStrZero(fpCOOInicial,6), Colunas,'|') ) ;
    Add( PadSpace('COO do Ultimo Cupom:|'+IntToStrZero(fpCOOFinal,6),Colunas,'|'));
    Add( PadCenter(' Totalizadores ',Colunas,'-') ) ;
    Add( PadSpace('Totalizador Geral:|'+FormatFloat('###,###,##0.00', fpGrandeTotal ),Colunas,'|') ) ;

    For A := 0 To fpAliquotas.Count - 1 do
      BrutaDia := RoundTo(BrutaDia + fpAliquotas[A].Total, -2);

    Add( PadSpace('Venda Bruta Diaria:|'+FormatFloat('###,###,##0.00', BrutaDia), Colunas, '|'));

    Add( PadCenter('Total Vendido por Aliquota',Colunas,'-') ) ;
    Add( PadSpace('Substituicao Tributaria (FF)|'+FormatFloat('###,###,##0.00', fpAliquotas[0].Total ), Colunas,'|') ) ;
    Add( PadSpace('Isencao (II)|'+FormatFloat('###,###,##0.00', fpAliquotas[1].Total ), Colunas,'|') ) ;
    Add( PadSpace('Nao Incidencia (NN)|'+FormatFloat('###,###,##0.00', fpAliquotas[2].Total ), Colunas,'|') ) ;

    For A := 3 to fpAliquotas.Count - 1 do
    begin
      with fpAliquotas[A] do
      begin
        Add( PadSpace(IntToStrZero(A,2)+'|'+ Tipo + FormatFloat('#0.00',Aliquota)+'%|'+
             FormatFloat('###,###,##0.00',Total),Colunas,'|') ) ;
        TotalAliq := RoundTo(TotalAliq + Total,-2) ;
      end ;
    end;

    Add( PadSpace('Total Cancelado R$|'+FormatFloat('###,###,##0.00', fpCuponsCanceladosTotal), Colunas,'|') ) ;
    Add( PadSpace('T O T A L   R$|'+FormatFloat('###,###,##0.00',TotalAliq), Colunas,'|') ) ;
    Add( PadCenter(' Relatorio Gerencial ',Colunas,'-') ) ;
    Add( PadSpace(' Relatorio Geral:|'+IntToStrZero(fpNumCER,6),Colunas,'|') ) ;
    Add( PadCenter('Formas de Pagamento',Colunas,'-') ) ;

    For A := 0 to fpFormasPagamentos.Count - 1 do
    begin
      with fpFormasPagamentos[A] do
      begin
        Add( PadSpace(Indice+'  '+PadRight(Descricao,20)+'|'+
             FormatFloat('###,###,##0.00',Total), Colunas,'|') ) ;
      end ;
    end ;

    Add( PadCenter('Comprovantes nao Fiscal',Colunas,'-') ) ;
    For A := 0 to fpComprovantesNaoFiscais.Count - 1 do
    begin
      with fpComprovantesNaoFiscais[A] do
      begin
        Add( PadSpace(Indice+'  '+PadRight(Descricao,20)+'|'+
             FormatFloat('###,###,##0.00',Total), Colunas,'|') ) ;
      end ;
    end ;
  end ;

  AddBufferRodape ;
end;

procedure TACBrECFVirtualPrinterClass.InsertBufferCabecalho;
Var
  A : Integer ;
  V, Linha : AnsiString ;
begin
  if fpVerao then
    V := 'V'
  else
    V := ' ' ;

  For A := 0 to fsCabecalho.Count - 1 do
  begin
    Linha := PadCenter(fsCabecalho[A], Colunas) ;
    if A = 0 then
      Linha := fsCmdImpCondensado + Linha ;

    fsBuffer.Insert( A, Linha ) ;
  end ;

  fsBuffer.Insert( fsCabecalho.Count,
                   PadSpace('CNPJ: '+CNPJ+'|IE: '+IE, Colunas, '|') );

  fsBuffer.Insert( fsCabecalho.Count+1,
                   PadSpace( DateToStr(now)+' '+TimeToStr(now)+V+'|COO:'+
                   IntToStrZero(fpNumCupom,6), Colunas, '|' ) ) ;
end;

procedure TACBrECFVirtualPrinterClass.AddBufferCabecalho_Item;
Var
  A : Integer ;
begin
  fsBuffer.Add( PadCenter('COMPROVANTE  * NAO FISCAL *',Colunas) ) ;

  For A := 0 to fsCabecalhoItem.Count - 1 do
    fsBuffer.Add( fsCabecalhoItem[A] ) ;
end;

procedure TACBrECFVirtualPrinterClass.AddBufferRodape;
Var
  V : AnsiString ;
  A : Integer ;
  SL : TStringList;
begin
  if fpPAF <> '' then
  begin
    SL := TStringList.Create;
    try
      SL.Text := StringReplace(fpPAF,'|',sLineBreak,[rfReplaceAll]);
      fsBuffer.AddStrings( SL );
    finally
      SL.Free
    end ;
  end ;

  if fpVerao then
    V := 'V'
  else
    V := ' ' ;

//....+....1....+....2....+....3....+....4....+...
//N.Serie XXXXXXXXXXXXXXX Maq 999 v0.8.3b
//Oper. XXXXXXXXXXXXXXX 99/99/99 99:99:99V
//** N A O   E   C U P O M   F I S C A L **
  with fsBuffer do
  begin
    Add( StringOfChar('-',Colunas) ) ;
    Add( PadSpace('N.Serie '+PadRight(fpNumSerie,21)+'|Maq '+GetNumECF+'|'+
              'v'+NumVersao,Colunas,'|') );
    Add( PadSpace('Oper. '+PadRight(Operador,15)  +'|'+
              FormatDateTime('dd/mm/yy hh:nn:ss',now)+V,  Colunas,'|') );
    Add( PadCenter('** N A O   E   C U P O M   F I S C A L **',Colunas) );
    Add( StringOfChar('=',Colunas) ) ;
    For A := 1 to LinhasEntreCupons do
      Add( '' ) ;
  end ;
end;

procedure TACBrECFVirtualPrinterClass.AddBufferLinhas(AString: AnsiString);
var
  Linhas: TStringList;
begin
  Linhas := TStringList.Create;
  try
    Linhas.Text := AjustaLinhaColunas(AString);
    fsBuffer.AddStrings( Linhas );
  finally
    Linhas.Free;
  end;
end;

function TACBrECFVirtualPrinterClass.ColunasExpandido: Integer;
begin
  if ( fsCmdImpExpandidoUmaLinha = '' ) then
    Result := Colunas
  else
    Result := Round( Colunas / 2 );
end;

function TACBrECFVirtualPrinterClass.AjustaLinhaColunas(Linha: AnsiString
  ): AnsiString;
begin
  Result := AjustaLinhas( Linha, Colunas );
  Result := StringReplace( Result, #10, sLineBreak, [rfReplaceAll] ) ;
end;

procedure TACBrECFVirtualPrinterClass.AbreDocumento ;
begin
  if not EmLinha() then
    raise EACBrECFERRO.Create(ACBrStr('Impressora: '+fpModeloStr+' não está pronta.')) ;

  inherited ;
end;

procedure TACBrECFVirtualPrinterClass.AbreDocumentoVirtual;
begin
  InsertBufferCabecalho ;
  GravaBuffer ;
  ImprimeBuffer;
end;

procedure TACBrECFVirtualPrinterClass.EnviaConsumidorVirtual;
begin
  if Consumidor.Documento <> '' then
  begin
    fsBuffer.Add( StringofChar('-',Colunas) ) ;
    fsBuffer.Add(PadRight('CPF/CNPJ consumidor: '+Consumidor.Documento,Colunas)) ;

    if Consumidor.Nome <> '' then
      fsBuffer.Add(PadRight('Nome: '+Consumidor.Nome,Colunas)) ;

    if Consumidor.Endereco <> '' then
      fsBuffer.Add(PadRight('Endereco: '+Consumidor.Endereco,Colunas)) ;

    fsBuffer.Add( StringofChar('-',Colunas) ) ;

    Consumidor.Enviado := True ;
  end ;
end;

procedure TACBrECFVirtualPrinterClass.CancelaItemVendidoVirtual(NumItem: Integer);
begin
  ZeraBuffer;
  fsBuffer.Add( 'CANCELADO ITEM: '+IntToStrZero( NumItem,3) ) ;
  ImprimeBuffer ;
end;

procedure TACBrECFVirtualPrinterClass.SubtotalizaCupomVirtual(
  DescontoAcrescimo: Double; MensagemRodape: AnsiString);
var
  S: String;
begin
  ZeraBuffer;
  if DescontoAcrescimo <> 0 then
  begin
    if DescontoAcrescimo < 0 then
      S := 'Desconto '
    else
      S := 'Acrescimo' ;

    fsBuffer.Add( PadSpace('SUBTOTAL   R$|'+
                  FormatFloat('#,###,##0.00',SubTotal), Colunas,'|') ) ;
    fsBuffer.Add( PadSpace(S+'  R$|'+FormatFloat('#,###,##0.00',DescontoAcrescimo),
                       Colunas,'|') ) ;
  end ;

  fsBuffer.Add(  fsCmdImpExpandidoUmaLinha +
                 PadSpace( 'TOTAL  R$|'+FormatFloat('#,###,##0.00',
                       SubTotal+DescontoAcrescimo),
	               ColunasExpandido ,'|') +
                 fsCmdImpFimExpandido ) ;
  ImprimeBuffer ;
end;

procedure TACBrECFVirtualPrinterClass.EfetuaPagamentoVirtual(
  Pagto: TACBrECFVirtualClassPagamentoCupom);
var
  FPG: TACBrECFFormaPagamento;
  Troco: Double;
  Obs: String;
begin
  ZeraBuffer;
  FPG := fpFormasPagamentos[ Pagto.PosFPG ] ;

  fsBuffer.Add( PadSpace(FPG.Descricao+'|'+
                FormatFloat('#,###,##0.00', Pagto.ValorPago), Colunas,'|') ) ;

  Obs := Pagto.Observacao;
  while Obs <> '' do
  begin
    fsBuffer.Add( copy(Obs, 1, Colunas) ) ;
    Obs := copy(Obs, Colunas + 1, length(Obs) ) ;
  end ;

  if TotalPago >= SubTotal then   { Ultrapassou o Valor do Cupom }
  begin
    if fpPagamentosCupom.Count > 0 then
      fsBuffer.Add( PadSpace('SOMA  R$|'+FormatFloat('#,###,##0.00', TotalPago), Colunas, '|') );

     if TotalPago > SubTotal then  { Tem TROCO ? }
     begin
        Troco  := RoundTo(TotalPago - SubTotal,-2) ;
        fsBuffer.Add( fsCmdImpExpandidoUmaLinha +
                      PadSpace('TROCO  R$|'+FormatFloat('#,###,##0.00',Troco),
		      ColunasExpandido() ,'|' ) + fsCmdImpFimExpandido ) ;
     end ;
  end ;

  ImprimeBuffer ;
end;

procedure TACBrECFVirtualPrinterClass.FechaCupom(Observacao: AnsiString; IndiceBMP : Integer);
begin
  ZeraBuffer;
  inherited;
end;

procedure TACBrECFVirtualPrinterClass.FechaCupomVirtual(Observacao: AnsiString;
  IndiceBMP: Integer);
begin
  fsBuffer.Add( AjustaLinhaColunas(Observacao) ) ;
  AddBufferRodape ;

  ImprimeBuffer ;
end;

procedure TACBrECFVirtualPrinterClass.CancelaCupomVirtual;
begin
  ZeraBuffer;
  fsBuffer.Add( fsCmdImpExpandidoUmaLinha +
		PadCenter('*** CUPOM CANCELADO ***', ColunasExpandido() ) +
                fsCmdImpFimExpandido ) ;

  case Estado of
    estVenda :
      begin
        fsBuffer.Insert(0, fsCmdImpExpandidoUmaLinha +
                           PadSpace('TOTAL  R$|'+FormatFloat('#,###,##0.00',Subtotal),
                           ColunasExpandido(), '|') +
                           fsCmdImpFimExpandido ) ;
        AddBufferRodape ;
      end ;

    estPagamento, estNaoFiscal :
      AddBufferRodape ;
  else
    begin
      InsertBufferCabecalho;

      fsBuffer.Add( PadSpace('COO do Cupom Cancelado:|'+IntToStrZero(StrToInt(NumCupom)-1,6),
                         Colunas,'|') ) ;
      fsBuffer.Add( PadSpace('Valor da Operacao  R$:|'+
                         FormatFloat('#,###,##0.00',Subtotal),Colunas,'|') );
      fsBuffer.Add( '' ) ;
      fsBuffer.Add( 'Nome do Consumidor:' ) ;
      fsBuffer.Add( '' ) ;
      fsBuffer.Add( 'Telefone do Cosumidor:' ) ;
      fsBuffer.Add( '' ) ;

      AddBufferRodape ;
    end ;
  end;

  GravaBuffer ;
  ImprimeBuffer;
end;

procedure TACBrECFVirtualPrinterClass.INItoClass( ConteudoINI: TStrings ) ;
Var
  Ini : TMemIniFile ;
begin
  inherited INItoClass( ConteudoINI ) ;

  Ini := TMemIniFile.Create( '' ) ;
  try
    Ini.Clear;
    Ini.SetStrings( ConteudoINI );

    fsCmdGaveta := AscToString( Ini.ReadString('Impressora',
         'Comando_Abrir_Gaveta', cCmdGaveta) ) ;
    fsCmdCortaPapelCompleto := AscToString( Ini.ReadString('Impressora',
         'Comando_Corta_Papel_Completo', cCmdCortaPapelCompleto) ) ;
    fsCmdCortaPapelParcial := AscToString( Ini.ReadString('Impressora',
         'Comando_Corta_Papel_Parcial', cCmdCortaPapelParcial) ) ;
    fsCmdImpZera := AscToString( Ini.ReadString('Impressora',
         'Comando_Incializacao', cCmdImpZera) ) ;
    fsCmdImpCondensado := AscToString( Ini.ReadString('Impressora',
         'Comando_Ativar_Condensado', cCmdImpCondensado) ) ;
    fsCmdImpExpandidoUmaLinha := AscToString( Ini.ReadString('Impressora',
         'Comando_Expandido_uma_Linha', cCmdImpExpandidoUmaLinha) );
    fsCmdImpFimExpandido := AscToString( Ini.ReadString('Impressora',
         'Comando_Fim_Expandido', cCmdImpFimExpandido) );
  finally
    Ini.Free ;
  end ;

  ZeraBuffer;
end ;

procedure TACBrECFVirtualPrinterClass.ClasstoINI( ConteudoINI: TStrings );
var
  Ini: TMemIniFile;
begin
  inherited ClasstoINI( ConteudoINI ) ;

  Ini := TMemIniFile.Create( '' ) ;
  try
    Ini.Clear;
    Ini.SetStrings( ConteudoINI );

    Ini.WriteString('Impressora','Comando_Abrir_Gaveta', StringToAsc(fsCmdGaveta) ) ;
    Ini.WriteString('Impressora','Comando_Corta_Papel_Completo', StringToAsc(fsCmdCortaPapelCompleto) ) ;
    Ini.WriteString('Impressora','Comando_Corta_Papel_Parcial', StringToAsc(fsCmdCortaPapelParcial) ) ;
    Ini.WriteString('Impressora','Comando_Incializacao', StringToAsc(fsCmdImpZera) ) ;
    Ini.WriteString('Impressora','Comando_Ativar_Condensado', StringToAsc(fsCmdImpCondensado) );
    Ini.WriteString('Impressora','Comando_Expandido_uma_Linha', StringToAsc(fsCmdImpExpandidoUmaLinha) ) ;
    Ini.WriteString('Impressora','Comando_Fim_Expandido', StringToAsc(fsCmdImpFimExpandido) ) ;

    ConteudoINI.Clear;
    Ini.GetStrings( ConteudoINI );
  finally
    Ini.Free;
  end;
end ;

procedure TACBrECFVirtualPrinterClass.ImprimePorta(AString : AnsiString) ;
Var
  OldAguardandoResposta : Boolean ;
begin
  OldAguardandoResposta := AguardandoResposta ;
  AguardandoResposta    := True ;
  try
    fpDevice.EnviaString( AString );

    repeat
      Sleep(IntervaloAposComando);
    until fpDevice.EmLinha() ;
  finally
    AguardandoResposta := OldAguardandoResposta ;
  end ;
end ;

procedure TACBrECFVirtualPrinterClass.ImprimePorta(AStringList : TStringList) ;
Var
  A, L: Integer ;
  Buf : AnsiString ;
begin
  AguardandoResposta := True ;
  try
    if MaxLinhasBuffer < 1 then
      ImprimePorta( AStringList.Text )
    else
    begin
      Buf := '' ;
      L   := 0 ;
      For A := 0 to AStringList.Count - 1 do
      begin
        Buf := Buf + AStringList[A] + CRLF ;
        Inc( L );

        if L >= MaxLinhasBuffer then
        begin
          ImprimePorta( Buf );
          Buf := '';
          L   := 0 ;
        end;
      end;
    end;
  finally
    AguardandoResposta := False ;
  end ;
end ;

procedure TACBrECFVirtualPrinterClass.AbreCupom ;
begin
  ZeraBuffer ;
  AddBufferCabecalho_Item ;

  inherited ;
end;

procedure TACBrECFVirtualPrinterClass.VendeItemVirtual(
  ItemCupom: TACBrECFVirtualClassItemCupom);
var
  Aliq: TACBrECFAliquota;
  Linha, AliqStr, StrQtd, StrPreco, StrDescAcre : String;
  Total, PorcDesc : Double;
begin
  Aliq := fpAliquotas[ ItemCupom.PosAliq ];

  if Aliq.Aliquota > 0 then
    AliqStr := PadCenter(Aliq.Tipo + FormatFloat('#0.00',Aliq.Aliquota)+'%',7)
  else
    AliqStr := PadCenter(Aliq.Indice,7) ;

  if ItemCupom.Qtd = Round( ItemCupom.Qtd ) then
    StrQtd := FormatFloat('#######0', ItemCupom.Qtd )
  else
    StrQtd := FormatFloat('###0.000', ItemCupom.Qtd ) ;

  if RoundTo( ItemCupom.ValorUnit, -2 ) = ItemCupom.ValorUnit then
    StrPreco := FormatFloat('#####0.00', ItemCupom.ValorUnit )
  else
    StrPreco := FormatFloat('####0.000', ItemCupom.ValorUnit ) ;

  Total   := RoundABNT( ItemCupom.Qtd * ItemCupom.ValorUnit, -2) ;
  PorcDesc:= 0 ;
  StrDescAcre := '';
  if ItemCupom.DescAcres <> 0 then
  begin
    PorcDesc := abs( ItemCupom.DescAcres ) / Total * 100  ;

    if ItemCupom.DescAcres > 0 then
      StrDescAcre := 'ACRESCIMO'
    else
      StrDescAcre := 'DESCONTO' ;
  end ;

  with ItemCupom do
  begin
    Linha := fsMascaraItem ;
    Linha := StuffMascaraItem( Linha, fsMascaraItem, 'I', IntToStrZero(Sequencia,3)) ;
    Linha := StuffMascaraItem( Linha, fsMascaraItem, 'C', Codigo ) ;
    Linha := StuffMascaraItem( Linha, fsMascaraItem, 'D', Descricao ) ;
    Linha := StuffMascaraItem( Linha, fsMascaraItem, 'Q', StrQtd ) ;
    Linha := StuffMascaraItem( Linha, fsMascaraItem, 'U', Unidade ) ;
    Linha := StuffMascaraItem( Linha, fsMascaraItem, 'V', StrPreco ) ;
    Linha := StuffMascaraItem( Linha, fsMascaraItem, 'A', AliqStr ) ;
    Linha := StuffMascaraItem( Linha, fsMascaraItem, 'T', FormatFloat('###,##0.00', Total ), True ) ;
  end;

  ZeraBuffer;

  { Quebrando a linha pela COLUNA }
  Linha := Trim( Linha ) ;
  while Linha <> '' do
  begin
    fsBuffer.Add( copy(Linha,1,Colunas) ) ;
    Linha := copy(Linha,Colunas + 1, Length(Linha) ) ;
  end ;

  if StrDescAcre <> '' then
  begin
    Total := RoundTo(Total + ItemCupom.DescAcres, -2) ;
    fsBuffer.Add( PadSpace('|'+StrDescAcre+'|'+FormatFloat('#0.00', PorcDesc)+'%|R$ '+
                  FormatFloat('##,##0.00', ItemCupom.DescAcres)+'|'+
                  FormatFloat('###,##0.00',Total),Colunas,'|') ) ;
  end ;

  ImprimeBuffer ;
end;

procedure TACBrECFVirtualPrinterClass.LeituraX ;
begin
  ZeraBuffer;
  inherited;
end;

procedure TACBrECFVirtualPrinterClass.LeituraXVirtual;
begin
  fsBuffer.Add( fsCmdImpExpandidoUmaLinha +
                PadCenter('LEITURA X', ColunasExpandido() ) +
                fsCmdImpFimExpandido );

  AddBufferRelatorio ;
end;

procedure TACBrECFVirtualPrinterClass.ReducaoZ(DataHora : TDateTime) ;
begin
  ZeraBuffer;
  inherited;
end;

procedure TACBrECFVirtualPrinterClass.ReducaoZVirtual(DataHora: TDateTime);
begin
  fsBuffer.Add( fsCmdImpExpandidoUmaLinha +
                PadCenter('REDUCAO Z', ColunasExpandido() ) +
                fsCmdImpFimExpandido );

  AddBufferRelatorio ;
end;

procedure TACBrECFVirtualPrinterClass.LinhaRelatorioGerencial(Linha: AnsiString; IndiceBMP: Integer);
begin
  ImprimePorta( AjustaLinhaColunas(Linha) );
end;

procedure TACBrECFVirtualPrinterClass.LinhaCupomVinculado(Linha: AnsiString);
begin
  LinhaRelatorioGerencial( Linha );
end;

procedure TACBrECFVirtualPrinterClass.CortaPapel(const CorteParcial : Boolean) ;
begin
  if CorteParcial Then
    ImprimePorta( CmdCortaPapelParcial )
  else
    ImprimePorta( CmdCortaPapelCompleto ) ;

  Sleep(100);
end ;

procedure TACBrECFVirtualPrinterClass.AbreNaoFiscalVirtual(CPF_CNPJ: String;
  Nome: String; Endereco: String);
begin
  ZeraBuffer;
  CPF_CNPJ := trim(CPF_CNPJ) ;
  if CPF_CNPJ <> '' then
  begin
     fsBuffer.Add( StringofChar('-',Colunas) ) ;
     fsBuffer.Add(PadRight('CPF/CNPJ Consumidor: '+CPF_CNPJ,Colunas)) ;
     if Nome <> '' then
        fsBuffer.Add(PadRight('Nome: '+Nome,Colunas)) ;
     if Endereco <> '' then
        fsBuffer.Add(PadRight('Endereco: '+Endereco,Colunas)) ;
     fsBuffer.Add( StringofChar('-',Colunas) ) ;
  end ;
end;

procedure TACBrECFVirtualPrinterClass.RegistraItemNaoFiscalVirtual(
  CNF: TACBrECFComprovanteNaoFiscal; Valor: Double; Obs: AnsiString);
begin
  ZeraBuffer ;
  fsBuffer.Add( PadSpace( IntToStrZero(fpCNFCupom.Count + 1,3) +'|'+
                      CNF.Descricao +'|'+
                      FormatFloat('#,###,##0.00',Valor), Colunas, '|'  ) ) ;

  AddBufferLinhas( Obs );
  ImprimeBuffer ;
end;

procedure TACBrECFVirtualPrinterClass.AbreRelatorioGerencialVirtual(Indice: Integer);
begin
  ZeraBuffer ;
  fsBuffer.Add( PadCenter('GNF:'+IntToStrZero(fpNumGNF,6) +'         '+
                'GRG:'+IntToStrZero(fpNumGRG,6),Colunas) ) ;
  fsBuffer.Add( StringOfChar('-',Colunas) ) ;
  fsBuffer.Add( PadCenter('NAO E DOCUMENTO FISCAL',Colunas) ) ;

  fsBuffer.Add( fsCmdImpExpandidoUmaLinha +
		PadCenter('RELATORIO GERENCIAL', ColunasExpandido() ) +
                fsCmdImpFimExpandido );
  fsBuffer.Add( '' ) ;
end;

procedure TACBrECFVirtualPrinterClass.AbreCupomVinculadoVirtual(COO: String;
  FPG: TACBrECFFormaPagamento; CodComprovanteNaoFiscal: String; Valor: Double);
begin
  ZeraBuffer ;
  fsBuffer.Add( PadCenter('GNF:'+IntToStrZero(fpNumGNF,6) +'         '+
                'CDC:'+IntToStrZero(fpNumCDC,6),Colunas) ) ;
  fsBuffer.Add( '' ) ;
  fsBuffer.Add( PadCenter('COMPROVANTE NAO FISCAL VINCULADO',Colunas) ) ;
  fsBuffer.Add( '' ) ;
  fsBuffer.Add( PadSpace('COO do documento de compra:|' + COO, Colunas,'|') ) ;
  fsBuffer.Add( PadSpace('VALOR TOTAL DA COMPRA   R$:|'+
                FormatFloat('##,###,##0.00',fpSubTotal),Colunas,'|') ) ;
  fsBuffer.Add( PadSpace(PadRight(FPG.Descricao,23)+' R$:|'+
                FormatFloat('##,###,##0.00',Valor),Colunas,'|') ) ;
  fsBuffer.Add( '' ) ;
end;

procedure TACBrECFVirtualPrinterClass.FechaRelatorioVirtual;
begin
  ZeraBuffer ;
  AddBufferRodape ;
  ImprimeBuffer ;
end;

procedure TACBrECFVirtualPrinterClass.AbreGaveta ;
begin
  ImprimePorta( CmdGaveta );
end ;

end.

