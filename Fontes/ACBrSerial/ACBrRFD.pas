{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Maicon da Silva Evangelista                     }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
|* 10/04/2008: Daniel Simoes de Almeida
|*  - Adicionado campo [ECF] -> DataHoraSwBasico= em ACBRRFD.INI
|*    Se preenchido, será informado no campo Data/Hora da gravação do SB do
|*    registro E01... Se não informado grava campo com vazios
******************************************************************************}

{$I ACBr.inc}

unit ACBrRFD;

interface
uses ACBrBase, ACBrConsts,
     SysUtils , Classes, Contnrs, ACBrEAD
     {$IFDEF LINUX}
       {$IFNDEF FPC}
         ,Libc
       {$else}
         ,BaseUnix
       {$endif}
     {$ENDIF}
     {$IFNDEF COMPILER6_UP} ,ACBrD5 {$ENDIF};

const
   cRFDAtoCotepe  = 'PC5207 01.00.00' ;
   cRFDTipoECF    = 'ECF-IF ' ;

   cRFDArqINI     = 'ACBrRFD.ini' ;
   cRFDArqCupom   = 'cupom.ini' ;
   cRFDArqE14     = 'e14.txt' ;
   cRFDArqE15     = 'e15.txt' ;
   cRFDArqE16     = 'e16.txt' ;
   cRFDArqE21     = 'e21.txt' ;
   cRFDArqIDINI   = 'rfdid.ini' ;

type

EACBrRFDDirInvalido   = class(Exception) ;
EACBrRFDSH_Invalido   = class(Exception) ;
EACBrRFDINIIncompleto = class(Exception) ;

{ Evento para retornar a chave para calculo interno do Hash de linha de Log.
  Se não programado, usará o CNPJ do ECF como chave }
TACBrRFDGetKeyHashLog = procedure(var Chave : String) of object ;
{ Evento para calculo Externo do Hash de linha de Log. Se não programado, usará
  a rotina de calculo Interno }
TACBrRFDCalcHashLog = procedure(const Linha : String;
   var Hash : String) of object ;

TACBrRFDItemCupom = class
 private
    fsDescricao: String;
    fsValorUnitario: Double;
    fsQtd: Double;
    fsAliquota: String ;
    fsCodigo: String;
    fsDesconto:Double;
    fsCancelado: Char ;
    fsUnidade: String;
    fsAcrescimo: Double;
    procedure SetCancelado(const AValue: Char);
 public
    constructor create ;

    property Codigo    : String  read fsCodigo    write fsCodigo    ;
    property Descricao : String  read fsDescricao write fsDescricao ;
    property Qtd       : Double  read fsQtd       write fsQtd       ;
    property ValorUnitario : Double  read fsValorUnitario write fsValorUnitario ;
    property Unidade   : String  read fsUnidade   write fsUnidade ;
    property Aliquota  : String  read fsAliquota  write fsAliquota  ;
    property Desconto  : Double  read fsDesconto  write fsDesconto  ;
    property Acrescimo : Double  read fsAcrescimo write fsAcrescimo ;
    property Cancelado : Char    read fsCancelado write SetCancelado ;

end;

TACBrRFDPagamentoCupom = class
 private
    fsValorPago: Double;
    fsDescricao: String;
 public
    property Descricao : String  read fsDescricao write fsDescricao;
    property ValorPago : Double  read fsValorPago write fsValorPago;
end;


TACBrRFDCupom = class
  private
    fsOwner: TObject;
    
    fsCCF: Integer;
    fsCOO: Integer;
    fsSubTotal: Double;
    fsDesconto: Double;
    fsDataEmissao: TDateTime;
    fsTipoDesconto: Char;
    fsTipoAcrescimo: Char;
    fsAcrescimo: Double;
    fsOrdemDA: Char;
    fsCancelado: Char;
    fsTotalLiquido: Double;
    fsNomeConsumidor: String;
    fsDoctoConsumidor: String;

    fsItens: TObjectList;
    fsPagamentos: TObjectList;
    fsNomeArq: String;

    procedure SetOrdemDA(const AValue: Char);
    procedure SetTipoAcrescimo(const AValue: Char);
    procedure SetTipoDesconto(const AValue: Char);
    procedure SetCancelado(const AValue: Char);
    procedure SetDoctoConsumidor(const AValue: String);
    procedure SetNomeArq(const AValue: String);

    procedure ZeraCupom ;
    procedure Le ;
    procedure Grava ;

  public
    constructor create( AOwner : TObject ) ;
    destructor Destroy ; override ;

    property NomeArq : String read fsNomeArq write SetNomeArq ;

    property CCF           : Integer   read fsCCF           write fsCCF ;
    property COO           : Integer   read fsCOO           write fsCOO ;
    property DataEmissao   : TDateTime read fsDataEmissao   write fsDataEmissao ;
    property SubTotal      : Double    read fsSubTotal      write fsSubTotal ;
    property Desconto      : Double    read fsDesconto      write fsDesconto ;
    property TipoDesconto  : Char      read fsTipoDesconto  write SetTipoDesconto ;
    property Acrescimo     : Double    read fsAcrescimo     write fsAcrescimo ;
    property TipoAcrescimo : Char      read fsTipoAcrescimo write SetTipoAcrescimo ;
    property TotalLiquido  : Double    read fsTotalLiquido  write fsTotalLiquido ;
    property Cancelado     : Char      read fsCancelado     write SetCancelado ;
    property OrdemDA       : Char      read fsOrdemDA       write SetOrdemDA ;
    property NomeConsumidor : String read fsNomeConsumidor  write fsNomeConsumidor ;
    property DoctoConsumidor: String read fsDoctoConsumidor write SetDoctoConsumidor ;

    property Itens      : TObjectList read fsItens ;
    property Pagamentos : TObjectList read fsPagamentos ;

    Procedure Descarrega ;
    Procedure VendeItem( const Codigo, Descricao: String;
       const Qtd, ValorUnitario: Double; const Unidade: String;
       const ValorDescAcres: Double; const Aliquota: String) ;
    Procedure CancelaItemVendido(const NumItem: Integer) ;
    Procedure EfetuaPagamento(const DescricaoFormaPagto: String; Valor: Double ) ;
end ;

{ TACBrRFD }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
TACBrRFD = class( TACBrComponent )     { Componente ACBrRFD }
  private
    fsDirECF: String;
    fsDirECFLog: String;
    fsAtivo  : Boolean;

    fsECF : TACBrComponent ;
    fsDirRFD: String;

    fsCupom : TACBrRFDCupom ;

    FACBrEAD: TACBrEAD;       /// Classe usada para AssinarArquivo com assinatura EAD.
    fsOnCalcEAD : TACBrEADCalc ;

    fsSH_Linha1: String;
    fsSH_CNPJ: String;
    fsSH_IE: String;
    fsSH_IM: String;
    fsSH_Linha2: String;
    fsSH_NumeroAplicativo: String;
    fsSH_COO: String;
    fsSH_NomeAplicativo: String;
    fsSH_VersaoAplicativo: String;
    fsSH_RazaoSocial: String;

    fsDiaMov: TDateTime;
    fsAtoCotepe: String;

    fsECF_CROAtual: Integer;
    fsECF_RFDID: String;
    fsECF_NumSerie: String ;
    fsECF_DataHoraSwBasico: TDateTime;

    fsCONT_RazaoSocial: String;
    fsCONT_CNPJ: String;
    fsCONT_IE: String;
    fsCONT_NumUsuario: Integer;
    fsCONT_GTCadastro: Double;
    fsCONT_CROCadastro: Integer;
    fsCONT_Endereco: String;
    fsCONT_DataHoraCadastro: TDateTime;
    fsOnCalcHashLog: TACBrRFDCalcHashLog;
    fsOnGetKeyHashLog: TACBrRFDGetKeyHashLog;
    fsDirECFMes: String;
    fsArqReducaoZ: String;
    fsArqRFD: String;
    fsIgnoraEcfMfd: Boolean;

    Procedure GerarRFD ;

    procedure SetSH_CNPJ(const AValue: String);
    procedure SetSH_COO(const AValue: String);
    procedure SetSH_IE(const AValue: String);
    procedure SetSH_IM(const AValue: String);
    procedure SetSH_Linha1(const AValue: String);
    procedure SetSH_Linha2(const AValue: String);
    procedure SetSH_NomeAplicativo(const AValue: String);
    procedure SetSH_NumeroAplicativo(const AValue: String);
    procedure SetSH_RazaoSocial(const AValue: String);
    procedure SetSH_VersaoAplicativo(const AValue: String);
    procedure SetAtoCotepe(const AValue: String);
    procedure SetCONT_RazaoSocial(const AValue: String);
    procedure SetCONT_Endereco(const AValue: String);
    procedure SetCONT_CNPJ(const AValue: String);
    procedure SetCONT_IE(const AValue: String);
    procedure SetECF_RFDID(const AValue: String);
    procedure SetECF(const AValue: TACBrComponent);
    procedure SetAtivo(const AValue: Boolean);
    procedure SetDirRFD(const AValue: String);
    function GetDirRFD: String;
    function GetArqINI: String;

    function GetArqRFDID: String;
    Procedure GravaLog(const Arq : String; Linha : String );
    function VerificaHashLinhaLog(var Linha: String): Boolean;
    function CalcHashLinhaLog(Linha: String): String;

    procedure SetDiaMov(const AValue: TDateTime);
    property pDiaMov : TDateTime read fsDiaMov write SetDiaMov ;
    procedure LeUltimaReducaoZ;
    procedure VerificaNovoDia;

    function GetOnRFDGetKeyRSA: TACBrEADGetChave;
    procedure SetOnRFDGetKeyRSA (const AValue: TACBrEADGetChave);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create( AOwner : TComponent) ; override ;
    destructor Destroy ; override ;

    property DiaMov : TDateTime read fsDiaMov ;

    property DirECF    : String read fsDirECF ;
    property DirECFLog : String read fsDirECFLog ;
    property DirECFMes : String read fsDirECFMes ;

    property ArqRFDID : String read GetArqRFDID ;
    property ArqRFD   : String read fsArqRFD ;
    property ArqReducaoZ : String read fsArqReducaoZ ;

    procedure Ativar ;
    procedure Desativar ;
    property Ativo : Boolean read fsAtivo write SetAtivo ;

    Procedure VerificaParametros ;
    Function AchaRFDID( RFDID : String ) : String ;

    procedure LerINI ;
    procedure GravarINI ;
    property ArqINI : String read GetArqINI ;
    Procedure CriarArqRFDID( const NomeArq : String );

    Function NomeArqRFD( DtMov : TDatetime ) : String ;

    { DADOS DO ECF }
    property ECF_CROAtual : Integer read fsECF_CROAtual write fsECF_CROAtual ;
    property ECF_RFDID    : String  read fsECF_RFDID    write SetECF_RFDID ;
    property ECF_DataHoraSwBasico  : TDateTime  read fsECF_DataHoraSwBasico
       write fsECF_DataHoraSwBasico ;

    property AtoCotepe    : String  read fsAtoCotepe    write SetAtoCotepe ;

    { DADOS DO CONTRIBUINTE }
    property CONT_CNPJ : String read fsCONT_CNPJ write SetCONT_CNPJ ;
    property CONT_IE   : String read fsCONT_IE write SetCONT_IE ;
    property CONT_NumUsuario : Integer read fsCONT_NumUsuario
       write fsCONT_NumUsuario ;
    property CONT_RazaoSocial : String  read fsCONT_RazaoSocial
       write SetCONT_RazaoSocial ;
    property CONT_Endereco : String read fsCONT_Endereco
       write SetCONT_Endereco ;
    property CONT_DataHoraCadastro : TDateTime read fsCONT_DataHoraCadastro
       write fsCONT_DataHoraCadastro ;
    property CONT_CROCadastro : Integer read fsCONT_CROCadastro
       write fsCONT_CROCadastro ;
    property CONT_GTCadastro  : Double  read fsCONT_GTCadastro
       write fsCONT_GTCadastro ;

  published
    property ECF : TACBrComponent  read fsECF write SetECF ;
    property DirRFD : String read GetDirRFD write SetDirRFD ;
    property IgnoraEcfMfd : Boolean read fsIgnoraEcfMfd write fsIgnoraEcfMfd
       default True ;

    { Dados da Sw.House e do Aplicativo }
    property SH_CNPJ : String read fsSH_CNPJ write SetSH_CNPJ ;
    property SH_IE   : String read fsSH_IE   write SetSH_IE   ;
    property SH_IM   : String read fsSH_IM   write SetSH_IM   ;
    property SH_RazaoSocial    : String read fsSH_RazaoSocial
       write SetSH_RazaoSocial ;
    property SH_NomeAplicativo : String read fsSH_NomeAplicativo
       write SetSH_NomeAplicativo ;
    property SH_NumeroAplicativo : String read fsSH_NumeroAplicativo
       write SetSH_NumeroAplicativo ;
    property SH_VersaoAplicativo : String read fsSH_VersaoAplicativo
       write SetSH_VersaoAplicativo ;
    property SH_COO    : String read fsSH_COO     write SetSH_COO ;
    property SH_Linha1 : String read fsSH_Linha1  write SetSH_Linha1 ;
    property SH_Linha2 : String read fsSH_Linha2  write SetSH_Linha2 ;

    Procedure AbreCupom ;
    Procedure VendeItem(const Codigo, Descricao: String;
       const Qtd, ValorUnitario: Double; const Unidade: String;
       const ValorDescAcres: Double; const Aliquota: String ) ;
    Procedure SubTotalizaCupom(const DescontoAcrescimo: Double );
    Procedure FechaCupom ;
    Procedure CancelaCupom(const COO: Integer);
    Procedure CancelaItemVendido(const NumItem: Integer) ;

    Procedure ReducaoZ( const DadosReducaoZ : AnsiString ) ;

    Procedure Documento(Denominacao: String) ;
    Procedure EfetuaPagamento(const DescricaoFormaPagto: String; Valor: Double ) ;

    property OnGetKeyHashLog : TACBrRFDGetKeyHashLog
       read fsOnGetKeyHashLog write fsOnGetKeyHashLog ;
    property OnCalcHashLog : TACBrRFDCalcHashLog
        read  fsOnCalcHashLog write fsOnCalcHashLog ;
    property OnCalcEAD: TACBrEADCalc read fsOnCalcEAD write fsOnCalcEAD;
    property OnGetKeyRSA: TACBrEADGetChave read GetOnRFDGetKeyRSA write SetOnRFDGetKeyRSA;
end;

implementation
Uses ACBrECF,
    {$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
    {$IFNDEF COMPILER6_UP}
      FileCtrl,
    {$ELSE}
      StrUtils, DateUtils,
    {$ENDIF}
      Math, IniFiles, ACBrUtil;


{----------------------------- TACBrRFDItemCupom ------------------------------}

constructor TACBrRFDItemCupom.create;
begin
  fsCodigo        := '' ;
  fsDescricao     := '' ;
  fsValorUnitario := 0  ;
  fsQtd           := 0  ;
  fsAliquota      := '' ;
  fsDesconto      := 0  ;
  fsAcrescimo     := 0  ;
  fsCancelado     := 'N';
  fsUnidade       := '' ;
end;

procedure TACBrRFDItemCupom.SetCancelado(const AValue: Char);
begin
  if not CharInSet(UpCase(AValue) , ['S','N']) then
     raise Exception.Create(ACBrStr('Valores válidos para TACBrRFDItemCupom.Cancelado, "S" ou "N"')) ;

  fsCancelado := UpCase( AValue );
end;


{--------------------------------- TACBrRFDCupom --------------------------------}

constructor TACBrRFDCupom.create( AOwner : TObject) ;
begin
  fsOwner := AOwner ;
  
  fsItens      := TObjectList.create( true ) ;
  fsPagamentos := TObjectList.create( true ) ;

  ZeraCupom ;
end;

destructor TACBrRFDCupom.Destroy;
begin
  fsItens.Free ;
  fsPagamentos.Free ;

  inherited destroy ;
end;

procedure TACBrRFDCupom.ZeraCupom;
begin
  fsCCF             := 0 ;
  fsCOO             := 0 ;
  fsSubTotal        := 0 ;
  fsDesconto        := 0 ;
  fsDataEmissao     := 0 ;
  fsTipoDesconto    := 'V';
  fsTipoAcrescimo   := 'V';
  fsAcrescimo       := 0 ;
  fsOrdemDA         := 'D' ;
  fsCancelado       := 'N' ;
  fsTotalLiquido    := 0 ;
  fsNomeConsumidor  := '' ;
  fsDoctoConsumidor := '' ;

  fsItens.Clear ;
  fsPagamentos.Clear ;
end;

procedure TACBrRFDCupom.SetNomeArq(const AValue: String);
begin
  fsNomeArq := AValue;
  Le ;
end;

procedure TACBrRFDCupom.Le ;
Var Ini : TMemIniFile ;
    A   : Integer ;
    S,T : String ;
    ItemCupom            : TACBrRFDItemCupom ;
    PagamentoCupom       : TACBrRFDPagamentoCupom ;
begin

  ZeraCupom ;

  if not FileExists( fsNomeArq ) then
     exit ;

  Ini := TMemIniFile.Create( fsNomeArq ) ;
  try
     fsCOO            := Ini.ReadInteger('Cupom','COO',fsCOO) ;
     fsCCF            := Ini.ReadInteger('Cupom','CCF',fsCCF) ;
     try
        fsDataEmissao := StoD( Ini.ReadString('Cupom','DataEmissao',DtoS(fsDataEmissao)) ) ;
     except
        fsDataEmissao := now ;
     end ;
     fsDesconto       := Ini.ReadFloat('Cupom','Desconto',fsDesconto) ;
     fsTipoDesconto   := Ini.ReadString('Cupom','TipoDesconto',fsTipoDesconto)[1] ;
     fsAcrescimo      := Ini.ReadFloat('Cupom','Acrescimo',fsAcrescimo) ;
     fsTipoAcrescimo  := Ini.ReadString('Cupom','TipoAcrescimo',fsTipoAcrescimo)[1] ;
     fsOrdemDA        := Ini.ReadString('Cupom','OdemDA',fsOrdemDA)[1] ;
     fsCancelado      := Ini.ReadString('Cupom','Cancelado',fsCancelado)[1] ;
     fsSubTotal       := Ini.ReadFloat('Cupom','SubTotal',fsSubTotal) ;
     fsTotalLiquido   := Ini.ReadFloat('Cupom','TotalLiquido',fsTotalLiquido) ;
     fsNomeConsumidor := Ini.ReadString('Cupom','NomeConsumidor',fsNomeConsumidor) ;
     fsDoctoConsumidor:= Ini.ReadString('Cupom','DoctoConsumidor',fsDoctoConsumidor) ;

     A := 0 ;
     while true do
     begin
        S := 'Item_Cupom'+IntToStrZero( A, 3) ;
        T := Ini.ReadString( S ,'Descricao','*FIM*') ;

        if T = '*FIM*' then break ;

        ItemCupom := TACBrRFDItemCupom.Create ;
        ItemCupom.Descricao := T ;
        ItemCupom.Codigo    := Ini.ReadString( S ,'Codigo','') ;
        ItemCupom.Qtd       := Ini.ReadFloat( S ,'Qtd',0) ;
        ItemCupom.ValorUnitario := Ini.ReadFloat( S ,'ValorUnit',0) ;
        ItemCupom.Aliquota  := Ini.ReadString( S ,'Aliquota','') ;
        ItemCupom.Unidade   := Ini.ReadString( S ,'Unidade','') ;
        ItemCupom.Desconto  := Ini.ReadFloat( S ,'Desconto',0) ;
        ItemCupom.Cancelado := Ini.ReadString( S ,'Cancelado','N')[1] ;

        fsItens.Add( ItemCupom ) ;
        A := A + 1 ;
     end ;

     A := 0 ;
     while true do
     begin
        S := 'Pagamento_Cupom'+IntToStrZero( A, 2) ;
        T := Ini.ReadString( S ,'Descricao','*FIM*') ;

        if T = '*FIM*' then break ;

        PagamentoCupom := TACBrRFDPagamentoCupom.Create ;
        PagamentoCupom.Descricao := T ;
        PagamentoCupom.ValorPago := Ini.ReadFloat( S ,'ValorPago',0) ;

        fsPagamentos.Add( PagamentoCupom ) ;
        A := A + 1 ;
     end ;
  finally
     Ini.Free ;
  end ;
end;

procedure TACBrRFDCupom.Grava ;
Var Ini : TMemIniFile ;
    A   : Integer ;
    S   : String ;
begin

  {$IFDEF LINUX}
    {$IFDEF FPC}
      FpUmask( 0 ) ;
    {$ELSE}
      umask( 0 ) ;
    {$ENDIF}
  {$ENDIF}
  
  Ini := TMemIniFile.Create( fsNomeArq ) ;

  try
     Ini.WriteInteger('Cupom','COO',fsCOO) ;
     Ini.WriteInteger('Cupom','CCF',fsCCF) ;
     Ini.WriteString('Cupom','DataEmissao', DtoS(fsDataEmissao) ) ;
     Ini.WriteFloat('Cupom','Desconto',fsDesconto) ;
     Ini.WriteString('Cupom','TipoDesconto',fsTipoDesconto) ;
     Ini.WriteFloat('Cupom','Acrescimo',fsAcrescimo) ;
     Ini.WriteString('Cupom','TipoAcrescimo',fsTipoAcrescimo) ;
     Ini.WriteString('Cupom','OdemDA',fsOrdemDA) ;
     Ini.WriteString('Cupom','Cancelado',fsCancelado) ;
     Ini.WriteFloat('Cupom','SubTotal',fsSubTotal) ;
     Ini.WriteFloat('Cupom','TotalLiquido',fsTotalLiquido) ;
     Ini.WriteString('Cupom','NomeConsumidor',fsNomeConsumidor) ;
     Ini.WriteString('Cupom','DoctoConsumidor',fsDoctoConsumidor) ;

     A := 0 ;
     while true do
     begin
        S := 'Item_Cupom'+IntToStrZero( A, 3) ;

        if A <= fsItens.Count - 1 then
           with fsItens[A] as TACBrRFDItemCupom do
           begin
              Ini.WriteString( S ,'Codigo', Codigo ) ;
              Ini.WriteString( S ,'Descricao', Descricao );
              Ini.WriteFloat( S ,'Qtd', Qtd ) ;
              Ini.WriteFloat( S ,'ValorUnit', ValorUnitario ) ;
              Ini.WriteString( S ,'Aliquota', Aliquota) ;
              Ini.WriteString( S ,'Unidade', Unidade) ;
              Ini.WriteFloat( S ,'Desconto', Desconto) ;
              Ini.WriteString( S, 'Cancelado', Cancelado);
           end
        else
           if Ini.SectionExists( S ) then
              Ini.EraseSection( S )
           else
              break ;

        A := A + 1 ;
     end ;

     A := 0 ;
     while true do
     begin
        S := 'Pagamento_Cupom'+IntToStrZero( A, 2) ;

        if A <= fsPagamentos.Count - 1 then
           with fsPagamentos[A] as TACBrRFDPagamentoCupom do
           begin
              Ini.WriteString( S ,'Descricao', Descricao ) ;
              Ini.WriteFloat( S ,'ValorPago', ValorPago ) ;
           end
        else
           if Ini.SectionExists( S ) then
              Ini.EraseSection( S )
           else
              break ;

        A := A + 1 ;
     end ;
  finally
     Ini.UpdateFile ;
     Ini.Free ;
  end ;
end;


procedure TACBrRFDCupom.VendeItem(const Codigo, Descricao: String; const Qtd,
  ValorUnitario: Double; const Unidade: String;
  const ValorDescAcres: Double; const Aliquota: String);
 Var ItemCupom : TACBrRFDItemCupom ;
     Ini : TMemIniFile ;
     Linha : String ;
begin
  { Adcionando o Item Vendido no ObjectList }
  ItemCupom := TACBrRFDItemCupom.Create ;
  ItemCupom.Codigo    := Codigo ;
  ItemCupom.Descricao := Descricao ;
  ItemCupom.Qtd       := Qtd ;
  ItemCupom.ValorUnitario := ValorUnitario ;
  ItemCupom.Unidade   := Unidade ;
  ItemCupom.Aliquota  := Aliquota ;
  if ValorDescAcres < 0 then
     ItemCupom.Desconto  := abs(ValorDescAcres)
  else
     ItemCupom.Acrescimo := ValorDescAcres ;

  fsItens.Add( ItemCupom ) ;

  { Grava apenas os dados do Item para não sobrecarregar o sistema, gravando
    todo o INI a cada Item vendido }
  Ini := TMemIniFile.Create( fsNomeArq ) ;
  try
     Linha := 'Item_Cupom'+IntToStrZero( fsItens.Count-1, 3) ;
     Ini.WriteString( Linha ,'Codigo', ItemCupom.Codigo ) ;
     Ini.WriteString( Linha ,'Descricao', ItemCupom.Descricao );
     Ini.WriteFloat( Linha ,'Qtd', ItemCupom.Qtd ) ;
     Ini.WriteFloat( Linha ,'ValorUnit', ItemCupom.ValorUnitario ) ;
     Ini.WriteString( Linha ,'Aliquota', ItemCupom.Aliquota) ;
     Ini.WriteString( Linha ,'Unidade', ItemCupom.Unidade) ;
     Ini.WriteFloat( Linha ,'Desconto', ItemCupom.Desconto) ;
     Ini.WriteFloat( Linha ,'Acrescimo', ItemCupom.Acrescimo) ;
     Ini.WriteString( Linha , 'Cancelado', ItemCupom.Cancelado);
  finally
     Ini.UpdateFile ;
     Ini.Free ;
  end ;
end;

procedure TACBrRFDCupom.CancelaItemVendido(const NumItem: Integer);
  Var Ini : TMemIniFile ;
begin
  if (NumItem < 0) or (NumItem > fsItens.Count) then
     exit ;

  TACBrRFDItemCupom( fsItens[NumItem-1] ).Cancelado := 'S';

  { Grava apenas os dados do Item alterado, para não sobrecarregar o
    sistema, gravando todo o INI a cada Item Cancelado }
  Ini := TMemIniFile.Create( fsNomeArq ) ;
  try
     Ini.WriteString( 'Item_Cupom'+IntToStrZero( NumItem-1, 3) , 'Cancelado', 'S');
  finally
     Ini.UpdateFile ;
     Ini.Free ;
  end ;
end;


procedure TACBrRFDCupom.EfetuaPagamento(const DescricaoFormaPagto: String;
  Valor: Double);
 Var PagamentoCupom : TACBrRFDPagamentoCupom ;
     Ini : TMemIniFile ;
     Linha : String ;
begin
  { Adcionando o Item Vendido no ObjectList }
  PagamentoCupom := TACBrRFDPagamentoCupom.Create ;
  PagamentoCupom.Descricao := LeftStr( DescricaoFormaPagto, 15) ;
  PagamentoCupom.ValorPago := Valor ;

  fsPagamentos.Add( PagamentoCupom ) ;

  { Grava apenas os dados do Pagamento para não sobrecarregar o sistema, gravando
    todo o INI a cada Pagamento }
  Ini := TMemIniFile.Create( fsNomeArq ) ;
  try
     Linha := 'Pagamento_Cupom'+IntToStrZero( fsPagamentos.Count-1, 2) ;
     Ini.WriteString( Linha ,'Descricao', PagamentoCupom.Descricao ) ;
     Ini.WriteFloat( Linha ,'ValorPago', PagamentoCupom.ValorPago ) ;
  finally
     Ini.UpdateFile ;
     Ini.Free ;
  end ;
end;


procedure TACBrRFDCupom.Descarrega ;

  Function NumDec( ADouble : Double) : Byte ;
    Var DblStr : String ;
        P : Integer ;
  begin
    Result := 0 ;
    DblStr := Trim(FloatToStr(ADouble)) ;
    P      := Pos(DecimalSeparator,DblStr) ;

    if P > 0 then
       Result := Length(DblStr) - P ;

    Result := max(min(Result,3),2) ;
  end ;

 Var Linha, Hash, Linhas : String ;
     A       : Integer ;
     FlagArr : Char ;
     GNF     : String ;
     TotItem : Double ;
     Un      : String ;
     DecQtd, DecVal : Byte ;

begin
  if not FileExists( fsNomeArq ) then
  begin
     ZeraCupom ;
     exit ;
  end ;

  Linha := IntToStrZero( CCF, 6) +
           IntToStrZero( COO, 6) +
           DtoS( DataEmissao ) +
           IntToStrZero( Round(SubTotal     * 100), 14) +
           IntToStrZero( Round(Desconto     * 100), 13) + TipoDesconto  +
           IntToStrZero( Round(Acrescimo    * 100), 13) + TipoAcrescimo +
           IntToStrZero( Round(TotalLiquido * 100), 14) +
           Cancelado +
           StringOfChar('0',13) + // Cancelamento de Acrescimo no Subtotal
           OrdemDA +
           PadRight(NomeConsumidor, 40) +
           Poem_Zeros(DoctoConsumidor, 14) ;

  TACBrRFD(fsOwner).GravaLog( cRFDArqE14, Linha) ;

  with TACBrECF(TACBrRFD(fsOwner).fsECF) do
  begin
     if Arredonda then
        FlagArr := 'A'
     else
        FlagArr := 'T' ;

     GNF := IntToStrZero(StrToIntDef(NumGNF,0),6) ;
  end ;

  Linhas := '' ;
  for A := 0 to fsItens.Count -1 do
  begin
     with fsItens[A] as TACBrRFDItemCupom do
     begin
        TotItem := (Qtd * ValorUnitario) - Desconto + Acrescimo ;
        if FlagArr = 'A' then
           TotItem := RoundTo(TotItem,-2)
        else
           TotItem := TruncFix( TotItem * 100 ) / 100 ;

        if Trim(Unidade) = '' then
           Un := 'UN'
        else
           Un := Unidade ;

        DecQtd := NumDec(Qtd) ;
        DecVal := NumDec(ValorUnitario) ;

        Linha := IntToStrZero( COO, 6) +
                 IntToStrZero( CCF, 6) +
                 IntToStrZero( A+1, 3) +
                 PadRight(Codigo,14) +
                 PadRight(Descricao,100) +
                 IntToStrZero( Round( Qtd * Power(10,DecQtd) ), 7  ) +
                 PadRight( Un, 3) +
                 IntToStrZero( Round( ValorUnitario * Power(10,DecVal) ), 8  ) +
                 IntToStrZero( Round( Desconto  * 100),  8 ) +
                 IntToStrZero( Round( Acrescimo * 100),  8 ) +
                 IntToStrZero( Round( TotItem   * 100), 14 ) +
                 PadRight( Aliquota, 7) +
                 Cancelado +
                 StringOfChar('0',33) + // (Qtd + Valor cancelado) + Canc.Acrescimo
                 FlagArr +
                 IntToStr(DecQtd) +
                 IntToStr(DecVal) ;

//      TACBrRFD(fsOwner).GravaLog( cRFDArqE15, Linha) ;
        Hash := TACBrRFD(fsOwner).CalcHashLinhaLog( Linha ) ;
        if Hash <> '' then
           Linha := Linha + '|$|' + Hash ;
           
        Linhas := Linhas + Linha + sLineBreak ;
     end ;
  end ;

  if Linhas <> '' then
  begin
     Linhas := LeftStr(Linhas, Length(Linhas) - Length(sLineBreak) ) ;  // Remove ultimo CRLF

     {$IFDEF LINUX}
       {$IFDEF FPC}
         FpUmask( 0 ) ;
       {$ELSE}
         umask( 0 ) ;
       {$ENDIF}
     {$ENDIF}

     with TACBrRFD(fsOwner) do
        WriteToTXT( fsDirECFLog + PathDelim + cRFDArqE15 , Linhas , True );  { True para adicionar no final do arquivo }
  end ;


  for A := 0 to fsPagamentos.Count -1 do
  begin
     with fsPagamentos[A] as TACBrRFDPagamentoCupom do
     begin
        Linha := IntToStrZero( COO, 6) +
                 IntToStrZero( CCF, 6) +
                 GNF +
                 PadRight( Descricao, 15) +
                 IntToStrZero( Round( ValorPago*100), 13 ) +
                 'N' +                  // Flag de Estorno
                 StringOfChar('0',13) ; // valor Estornado

        TACBrRFD(fsOwner).GravaLog( cRFDArqE21, Linha) ;
     end ;
  end ;

  ZeraCupom ;
  SysUtils.DeleteFile( fsNomeArq ) ;
end;

procedure TACBrRFDCupom.SetOrdemDA(const AValue: Char);
begin
  if not CharInSet(UpCase(AValue) , ['D','A']) then
     raise Exception.Create(ACBrStr('Valores válidos para TACBrRFDE14.OrdemDA, "D" ou "A"')) ;

  fsOrdemDA := UpCase( AValue );
end;

procedure TACBrRFDCupom.SetTipoAcrescimo(const AValue: Char);
begin
  if not CharInSet(UpCase(AValue) , ['V','P']) then
     raise Exception.Create(ACBrStr('Valores válidos para TACBrRFDE14.TipoAcrescimo, "V" ou "P"')) ;

  fsTipoAcrescimo := UpCase( AValue );
end;

procedure TACBrRFDCupom.SetTipoDesconto(const AValue: Char);
begin
  if not CharInSet(UpCase(AValue) , ['V','P']) then
     raise Exception.Create(ACBrStr('Valores válidos para TACBrRFDE14.TipoDesconto, "V" ou "P"')) ;

  fsTipoDesconto := UpCase( AValue );
end;

procedure TACBrRFDCupom.SetCancelado(const AValue: Char);
begin
  if not CharInSet(UpCase(AValue) , ['S','N']) then
     raise Exception.Create(ACBrStr('Valores válidos para TACBrRFDE14.Cancelado, "S" ou "N"')) ;

  fsCancelado := UpCase( AValue );
end;

procedure TACBrRFDCupom.SetDoctoConsumidor(const AValue: String);
begin
  fsDoctoConsumidor := Trim( OnlyNumber( AValue ) ) ;
end;


{----------------------------------- TACBrRFD ---------------------------------}

constructor TACBrRFD.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  fsECF          := nil ;
  fsIgnoraEcfMfd := True ;

  fsCupom := TACBrRFDCupom.Create( self ) ;
  Desativar ;  { Inicializa variaveis de diretório e nome de arquivo }

  fsSH_RazaoSocial      := '' ;
  fsSH_CNPJ             := '' ;
  fsSH_IE               := '' ;
  fsSH_IM               := '' ;
  fsSH_NomeAplicativo   := '' ;
  fsSH_VersaoAplicativo := '' ;
  fsSH_NumeroAplicativo := '' ;
  fsSH_Linha1           := '' ;
  fsSH_Linha2           := '' ;
  fsSH_COO              := '' ;

  FACBrEAD := TACBrEAD.Create(Self); //Classe para fazer assinatura EAD
end;

destructor TACBrRFD.Destroy;
begin
  fsCupom.Free ;
  FACBrEAD.Free;

  inherited Destroy ;
end;

procedure TACBrRFD.SetAtivo(const AValue: Boolean);
begin
  if AValue then
     Ativar
  else
     Desativar ;
end;

Procedure TACBrRFD.Desativar ;
begin
  fsAtivo      := False ;
// fsDirRFD     := '' ;
  fsDirECF     := '' ;
  fsDirECFLog  := '' ;

  fsDiaMov      := 0  ;
  fsArqRFD      := '' ;
  fsArqReducaoZ := '' ;
  fsDirECFMes   := '' ;

  fsECF_CROAtual := 0  ;
  fsECF_RFDID    := '' ;
  fsECF_NumSerie := '' ;
  fsECF_DataHoraSwBasico := 0 ;

  fsAtoCotepe  := cRFDAtoCotepe ;

  fsCONT_RazaoSocial      := '' ;
  fsCONT_CNPJ             := '' ;
  fsCONT_IE               := '' ;
  fsCONT_Endereco         := '' ;
  fsCONT_NumUsuario       := -1  ;
  fsCONT_DataHoraCadastro := 0  ;
  fsCONT_CROCadastro      := -1  ;
  fsCONT_GTCadastro       := -1  ;
end ;

procedure TACBrRFD.Ativar ;
 Var wCNPJ, wIE: String ;
     wCRO : Integer ;
     wRFDID: String ;
begin
  if fsAtivo then exit ;

  Desativar ;

  {$IFDEF LINUX}
    {$IFDEF FPC}
      FpUmask( 0 ) ;
    {$ELSE}
      umask( 0 ) ;
    {$ENDIF}
  {$ENDIF}

  if not DirectoryExists( DirRFD ) then
     if not CreateDir( DirRFD ) then
        raise EACBrRFDDirInvalido.Create( 'Erro ao criar o diretório: '+DirRFD) ;

  { Verifica se precisa criar arquivo RFDID.INI }
  if not FileExists( ArqRFDID ) then
     CriarArqRFDID( ArqRFDID ) ;

  if not Assigned(fsECF) then
     raise Exception.Create( ACBrStr('ACBrRFD não está associado a ACBrECF'));

  if not TACBrECF(fsECF).Ativo then
     raise Exception.Create( ACBrStr('ACBrECF associado a ACBrRFD não está Ativo'));

  with TACBrECF( fsECF ) do
  begin
     fsECF_NumSerie := Trim(NumSerie) ; // Joga em variavel para evitar chamadas ao ECF

     fsDirECF := RightStr( fsECF_NumSerie, 11) ;  { Ultimos 11 digitos do Num.Serie }
     if Length(fsDirECF) > 8 then
        fsDirECF := LeftStr(fsDirECF,8)+'.'+copy(fsDirECF,9,3) ;

     fsDirECF := DirRFD + PathDelim + fsDirECF ;

     if not DirectoryExists( fsDirECF ) then
     begin
        if not CreateDir( fsDirECF ) then
           raise EACBrRFDDirInvalido.Create( 'Erro ao criar o diretório: '+fsDirECF) ;
     end ;

     { Criando diretório para o LOG diário }
     fsDirECFLog := fsDirECF + PathDelim + 'log' ;
     if not DirectoryExists( fsDirECFLog ) then
        if not CreateDir( fsDirECFLog ) then
           raise EACBrRFDDirInvalido.Create( 'Erro ao criar o diretório: '+fsDirECFLog) ;
     fsCupom.NomeArq := fsDirECFLog + PathDelim + cRFDArqCupom ;

     wCNPJ := '' ;                  
     wIE   := '' ;

     { Tenta ler informações diretamente do ECF, se não achar usará do INI }
     if (Modelo <> ecfSchalter) then    { Schalter demora quase 1 minuto para }
     begin
        wCNPJ := CNPJ ;
        wIE   := IE ;
     end ;
     wCRO   := StrToIntDef(NumCRO,0) ;
     wRFDID := RFDID ;

     { Existe INI de configuraçoes do ECF e Usuário ? Se não existir, crie... }
     if not FileExists(ArqINI) then
      begin
        CONT_CNPJ    := wCNPJ ;
        CONT_IE      := wIE ;
        ECF_CROAtual := wCRO ;
        ECF_RFDID    := wRFDID ;
        AtoCotepe    := cRFDAtoCotepe ;
        pDiaMov      := DateOf( DataMovimento ) ;  { Ajustando Dir/Arquivos para Novo Dia de Movimeto }

        { TODO: Criar Function em ACBrECF que captura Leitura
          Mem.Fiscal e tentar achar informações do cabeçalho RFD
          (ECF, Contribuinte, Num.Usuário )

          DadosCabecalhoRFD  }

        GravarINI ;
      end
     else
      begin
        LerINI ;

(*      - Verificação de RFDID comentada pois falha em ECFs OEM

        if Trim(wRFDID) <> '' then         { ECF retornou RFDID igual do INI ? }
           if pos( wRFDID, fsECF_RFDID ) = 0 then
              fsECF_RFDID := RFDID ;

        - Verificação de CNPJ e IE comentada pois tb falham em alguns ECFs

        if wCNPJ <> '' then     { Se ECF retornou essas informações, }
           CONT_CNPJ := wCNPJ ; { use-as ao inves dos dados do INI   }
        if wIE <> '' then
           CONT_IE := wIE ;
*)
        if wCRO <> 0 then
           ECF_CROAtual := wCRO ;
      end ;

     { Verifica se é novo Dia }
     VerificaNovoDia ;
  end ;

  fsAtivo := True ;
end;

procedure TACBrRFD.VerificaNovoDia ;
  Var DtMovECF : TDateTime ;
begin
  try
     DtMovECF := DateOf( TACBrECF(fsECF).DataMovimento ) ;
  except
     { alguns ECFs (como a Schalter) não retornam DataMovimento com Cupom Aberto }
     DtMovECF := DiaMov ;
  end ;

  { Verifica se é novo Dia }
  if DtMovECF <> DiaMov then
  begin
     { Gravou RFD do dia anterior ? Se NAO, grave agora antes de ativar }
     if not FileExists( ArqRFD ) then
     begin
        { Se não emitiu RZ do dia anterior, alguns ECFs emitem RZ automática }
        if not FileExists( ArqReducaoZ ) then
           LeUltimaReducaoZ ;

        GerarRFD ;
     end ;

     pDiaMov := DtMovECF ;  { Ajustando Dir/Arquivos para Novo Dia de Movimeto }
     GravarINI ;
  end ;

  { Criando diretório para arquivos do mes atual }
  if not DirectoryExists( DirECFMes ) then
     if not CreateDir( DirECFMes ) then
        raise EACBrRFDDirInvalido.Create( 'Erro ao criar o diretório: '+DirECFMes) ;
end ;

procedure TACBrRFD.VerificaParametros;
Var
  Erro, ErroECF, ErroCO, ErroCA, MM : String ;
begin
  if not fsAtivo then
     raise Exception.Create(ACBrStr('ACBrRFD não está Ativo'));

  if not Assigned(fsECF) then
     raise Exception.Create(ACBrStr('ACBrRFD não está associado a ACBrECF')) ;

  if not TACBrECF(fsECF).Ativo then
     raise Exception.Create(ACBrStr('ACBrECF não está Ativo'));

  if fsIgnoraEcfMfd then
     if TACBrECF(fsECF).MFD then
        raise Exception.Create(ACBrStr('ECFs com MFD devem gerar arquivo conforme CAT 17/04'));

  { Verificando se preencheu propriedades da Sw.House }
  if (SH_CNPJ = '')        or (SH_NomeAplicativo = '')   or
     (SH_RazaoSocial = '') or (SH_VersaoAplicativo = '') then
     raise EACBrRFDSH_Invalido.Create(
        'Dados da Software House incompletos. '+sLineBreak+sLineBreak+
        'Favor informar ao ACBrRFD os campos: '+sLineBreak+
        'SH_RazaoSocial, SH_CNPJ, '+sLineBreak+
        'SH_NomeAplicativo, SH_VersaoAplicativo' );

(*  Codigo comentado, pois essa verificação pode ser lenta e nao funcionar em W98 
  { Verificando se tem mecanismo de calculo do Registro EAD }
  if not Assigned( fsOnCalcEAD ) then
  begin
     ArqV := fsDirECFLog + PathDelim + 'ssl.ver' ;
     {$IFDEF MSWINDOWS}
       ArqB := fsDirECFLog + PathDelim + 'ssl.bat' ;
       { Criando BAT para chamar o "openssl.exe" com redirecionador ">" }
       WriteToTXT(ArqB,'openssl version > ' + ArqV, False) ;
       RunCommand(ArqB,'' ,True,0);
       SysUtils.DeleteFile(ArqB) ;
     {$ELSE}
       RunCommand('openssl' ,'version > '+ArqV , True);
     {$ENDIF}

      Sleep(300) ;
      if not FileExists( ArqV ) then
         raise Exception.Create(ACBrStr('Não existe método para calculo do registro EAD'+sLineBreak+
                               '(Instale o programa "openssl", disponível em:'+sLineBreak+
                               ' http://www.openssl.org/related/binaries.html )')) ;
      SysUtils.DeleteFile( ArqV ) ;
  end ;
*)

  { Verificando se Acha Marca e Modelo baseado em RFDID }
  Erro    := '' ;
  ErroECF := '' ;
  MM      := AchaRFDID(fsECF_RFDID) ;
  if MM = '' then
     ErroECF := 'RFDID ('+fsECF_RFDID+') não encontrado.' + sLineBreak
  else if pos('|',MM ) = 0 then
     ErroECF := 'RFDID ('+fsECF_RFDID+') não especifica o modelo.' + sLineBreak ;
  if ErroECF <> '' then
     ErroECF := ErroECF + 'Consulte o arquivo:' + sLineBreak+
                          ArqRFDID + sLineBreak ;

  { Verificando se dados do ECF estão preenchidos }
  if fsECF_CROAtual = 0 then
     ErroECF := ErroECF + 'CRO Atual do ECF, não definido '+sLineBreak ;
  if ErroECF <> '' then
     ErroECF := ErroECF + sLineBreak ;

  { Verificando Dados do Contribuinte }
  ErroCO := '' ;
  if fsCONT_RazaoSocial = '' then
     ErroCO := ErroCO + 'Razão Social, ' ;
  if fsCONT_CNPJ = '' then
     ErroCO := ErroCO + 'CNPJ, ' ;
  if fsCONT_Endereco = '' then
     ErroCO := ErroCO + 'Endereço, ' ;
  if ErroCO <> '' then
     ErroCO := ErroCO + sLineBreak + 'do Contribuinte, não definido(s).'+sLineBreak+
        '(Obtenha esses dados no Cabeçalho da Leitura X)'+sLineBreak+sLineBreak ;

  { Verificando Dados do Usuário Atual }
  ErroCA := '' ;
  if fsCONT_NumUsuario <= 0 then
     ErroCA := ErroCA + 'Numero de Usuário, '+sLineBreak ;
  if fsCONT_DataHoraCadastro = 0 then
     ErroCA := ErroCA + 'Data/Hora do Cadastro, '+sLineBreak ;
  if fsCONT_CROCadastro < 0 then
     ErroCA := ErroCA + 'CRO no momento do Cadastro, '+sLineBreak ;
//  if fsCONT_GTCadastro < 0 then
//     ErroCA := ErroCA + 'Grande Total no momento do Cadastro, '+sLineBreak ;
  if ErroCA <> '' then
     ErroCA := ErroCA + 'do Usuário atual do ECF, não definido(s).'+sLineBreak+
                        '(Obtenha esses dados a partir de uma Leitura da Memória Fiscal)' ;

  Erro := Trim(ErroECF + ErroCO + ErroCA) ;
  if Erro <> '' then
  begin
     Erro := 'Arquivo "'+cRFDArqINI +'" não configurado corretamente' +sLineBreak+
             ArqINI+sLineBreak +sLineBreak +
             Erro ;
     raise EACBrRFDINIIncompleto.Create(Erro);
  end ;
end;

procedure TACBrRFD.SetDiaMov(const AValue: TDateTime);
begin
  fsDiaMov      := DateOf( AValue );
  fsArqRFD      := NomeArqRFD( fsDiaMov ) ;
  fsDirECFMes   := ExtractFilePath( fsArqRFD ) ;
  fsArqReducaoZ := fsDirECFMes + PathDelim +
                   'rz'+FormatDateTime('yymmdd', fsDiaMov )+'.ini' ;
end;

function TACBrRFD.GetArqINI: String;
begin
  Result := '' ;
  if fsDirECF <> '' then
     Result := fsDirECF + PathDelim + cRFDArqINI
end;

function TACBrRFD.GetArqRFDID: String;
begin
  Result := '' ;
  if DirRFD <> '' then
     Result := fsDirRFD + PathDelim + cRFDArqIDINI
end;

function TACBrRFD.GetDirRFD: String;
begin
  if fsDirRFD = '' then
     if not (csDesigning in Self.ComponentState) then
        fsDirRFD := ExtractFilePath( ParamStr(0) ) + 'RFD' ;

  Result := fsDirRFD ;
end;

function TACBrRFD.GetOnRFDGetKeyRSA: TACBrEADGetChave;
begin
  Result := FACBrEAD.OnGetChavePrivada;
end;

procedure TACBrRFD.SetDirRFD(const AValue: String);
begin
  if fsDirRFD = AValue then exit ;

  if fsAtivo then
     raise Exception.Create(ACBrStr('DirRFD não pode ser modificado com o ACBrRFD Ativo'));

  fsDirRFD := PathWithoutDelim( AValue ) ;    { Remove ultimo PathDelim }
end;


procedure TACBrRFD.AbreCupom ;
begin
  fsCupom.Descarrega ;

  if DateOf( now ) > DiaMov then
     VerificaNovoDia ;

  with TACBrECF( fsECF ) do
  begin
     fsCupom.CCF             := StrToIntDef(NumCCF,0) ;
     fsCupom.COO             := StrToIntDef(NumCOO,0) ;
     fsCupom.DataEmissao     := now ;
     fsCupom.NomeConsumidor  := Consumidor.Nome ;
     fsCupom.DoctoConsumidor := Consumidor.Documento ;
  end ;

  fsCupom.Grava ;
end;

procedure TACBrRFD.SubTotalizaCupom(const DescontoAcrescimo: Double);
begin
  if DescontoAcrescimo > 0 then
     fsCupom.Acrescimo := DescontoAcrescimo
  else
     fsCupom.Desconto  := abs(DescontoAcrescimo) ;

  with TACBrECF( fsECF ) do
  begin
     fsCupom.TotalLiquido := Subtotal ;  // Lê SubTotal do ECF 
     fsCupom.SubTotal     := RoundTo( fsCupom.TotalLiquido - DescontoAcrescimo, -2) ;
  end ;

   fsCupom.Grava ;
end;


procedure TACBrRFD.FechaCupom ;
begin
  with TACBrECF( fsECF ) do
  begin
     if Consumidor.Atribuido then
     begin
        fsCupom.NomeConsumidor  := Consumidor.Nome ;
        fsCupom.DoctoConsumidor := Consumidor.Documento ;
     end ;
  end ;

  fsCupom.Grava ;
end;

procedure TACBrRFD.CancelaCupom(const COO: Integer);
 Var SL: TStringList ;
     Linha, Hash : String ;
     PosHash : Integer ;
begin
  if fsCupom.fsCOO = COO then
   begin
     fsCupom.Cancelado := 'S' ;
     fsCupom.Grava ;
     fsCupom.Descarrega ;
   end
  else
   begin
      SL := TStringList.Create;
      try
         SL.LoadFromFile( fsDirECFLog + PathDelim + cRFDArqE14 );
         Linha := SL.Strings[SL.Count - 1];
         Linha := StuffString(Linha,77,1,'S') ;  // Granvando Flag de Cancelado

         // Recalculando o Hash da linha
         PosHash := pos('|$|',Linha) ;
         if PosHash = 0 then
            PosHash := Length( Linha ) + 1 ;
         Linha := LeftStr(Linha,PosHash - 1) ;
         Hash := CalcHashLinhaLog( Linha ) ;
         if Hash <> '' then
            Linha := Linha + '|$|' + Hash ;

         SL.Strings[SL.Count -1] := Linha;
         SL.SaveToFile( fsDirECFLog + PathDelim + cRFDArqE14 );
      finally
         SL.Free ;
      end ;
   end;
end;

procedure TACBrRFD.VendeItem( const Codigo, Descricao: String;
       const Qtd, ValorUnitario: Double; const Unidade: String;
       const ValorDescAcres: Double; const Aliquota: String) ;
begin
  fsCupom.VendeItem( Codigo, Descricao, Qtd, ValorUnitario, Unidade,
                     ValorDescAcres, Aliquota );
end;

procedure TACBrRFD.CancelaItemVendido(const NumItem: Integer) ;
begin
  fsCupom.CancelaItemVendido( NumItem );
end;

procedure TACBrRFD.Documento(Denominacao: String) ;
  Var Linha, GRG, CDC, CRZ : String ;
begin
  fsCupom.Descarrega ;

  if DateOf( now ) > DiaMov then
     VerificaNovoDia ;

  with TACBrECF( fsECF ) do
  begin
     Denominacao := UpperCase(PadRight(Denominacao,2)) ;
     GRG := '000000' ;
     CRZ := GRG ;
     CDC := '0000' ;

     if Denominacao = 'RG' then
        GRG := IntToStrZero( StrToIntDef(NumGRG,0), 6)
     else if Denominacao = 'CC' then
        CDC := IntToStrZero( StrToIntDef(NumCDC,0), 4)
     else if Denominacao = 'RZ' then
        CRZ := IntToStrZero( StrToIntDef(NumCRZ,0), 6) ;

     Linha := IntToStrZero( StrToIntDef(NumCOO,0), 6) +
              IntToStrZero( StrToIntDef(NumGNF,0), 6) +
              GRG + CDC + CRZ + Denominacao +
              DTtoS( now ) ;

     GravaLog(cRFDArqE16, Linha);
  end ;
end;

procedure TACBrRFD.EfetuaPagamento(const DescricaoFormaPagto: String; Valor: Double ) ;
  Var Linha : String ;
begin
  if FileExists( fsCupom.NomeArq ) then  { Tem Cupom aberto ? }
     fsCupom.EfetuaPagamento( DescricaoFormaPagto, Valor )
  else
   begin
     with TACBrECF( fsECF ) do
     begin
        Linha := IntToStrZero( StrToIntDef(NumCOO,0), 6) +
                 IntToStrZero( StrToIntDef(NumCCF,0), 6) +
                 IntToStrZero( StrToIntDef(NumGNF,0), 6) +
                 PadRight( DescricaoFormaPagto, 15) +
                 IntToStrZero( Round( Valor*100), 13 ) +
                 'N' +                  // Flag de Estorno
                 StringOfChar('0',13) ; // valor Estornado
     end ;

     GravaLog( cRFDArqE21, Linha) ;
   end ;
end;

procedure TACBrRFD.ReducaoZ( const DadosReducaoZ : AnsiString ) ;
  Var Ini : TMemIniFile ;
      CRZ, COO : Integer ;
begin
  fsCupom.Descarrega ;
  Documento('RZ');

  WriteToTXT( ArqReducaoZ, DadosReducaoZ, False );

  // Atualizando CRZ e COO no arquivo da Redução Z, pois leu os dados antes de Emitir a Z
  with TACBrECF( fsECF ) do
  begin
     Ini := TMemIniFile.Create( ArqReducaoZ );
     try
         CRZ := Ini.ReadInteger('ECF', 'NumCRZ', 0);
         if CRZ = 0 then
            CRZ := StrToIntDef( NumCRZ,0)
         else
            CRZ := CRZ + 1 ;

         COO := Ini.ReadInteger('ECF', 'NumCOO', 0);
         if COO = 0 then
            COO := StrToIntDef( NumCOO,0)
         else
            COO := COO + 1 ;

         Ini.WriteInteger('ECF', 'NumCRZ', CRZ);
         Ini.WriteInteger('ECF', 'NumCOO', COO);
         Ini.WriteString('ECF','DataHoraEmissao', DTtoS(Now) );
     finally
        Ini.UpdateFile ;
        Ini.Free ;
     end ;
  end ;

  GerarRFD ;
end;

procedure TACBrRFD.LeUltimaReducaoZ ;
  Var Ini : TMemIniFile ;
      RZ, DTM  : String ;
      SL : TStringList ;
begin
  with TACBrECF( fsECF ) do
  begin
     RZ := '' ;
     try
        RZ := TACBrECF( fsECF ).DadosUltimaReducaoZ ;
     except
     end ;

     if RZ = '' then exit ;

     Ini := TMemIniFile.Create( ArqReducaoZ );
     SL  := TStringList.Create ;
     try
        SL.Clear ;
        SL.Text := RZ ;

        Ini.SetStrings(SL);

        DTM := Ini.ReadString('ECF','DataMovimento','') ;
{ Leu Redução Z do mesmo movimento que estava registrando ? Se NAO, cai fora.. }        
        if DTM <> FormatDateTime('dd/mm/yy',DiaMov) then
           exit ;

         Ini.WriteString('ECF','DataHoraEmissao', DTtoS(Now) );
         Ini.UpdateFile ;
     finally
        Ini.Free ;
        SL.Free ;
     end ;
  end ;
end ;

procedure TACBrRFD.GerarRFD ;
 Var Linha, CabOk, CabNot : String ;
     wNumSerie, wLetraMF, wNumUsuario, wTipoECF, wMarca, wModelo, wCNPJECF,
     wNumCRZ, wDiaMov, wDtHrSwBasico : String ;
     P, I : Integer ;
     SL, SEC : TStringList ;
     HashOk : Boolean ;
     {Dir,} ArqTmp, Arq : String ;
     Ini : TMemIniFile ;
begin
  {$IFDEF LINUX}
    {$IFDEF FPC}
      FpUmask( 0 ) ;
    {$ELSE}
      umask( 0 ) ;
    {$ENDIF}
  {$ENDIF}

  fsCupom.Descarrega ;

  if not ( FileExists(fsDirECFLog + PathDelim + cRFDArqCupom) or
           FileExists(fsDirECFLog + PathDelim + cRFDArqE14)   or
           FileExists(fsDirECFLog + PathDelim + cRFDArqE15)   or
           FileExists(fsDirECFLog + PathDelim + cRFDArqE16)   or
           FileExists(fsDirECFLog + PathDelim + cRFDArqE21)   or
           ( (fsArqReducaoZ <> '') and FileExists(fsArqReducaoZ) ) )  then
     exit ;

  { Criando arquivo RFD Temporário  }
  ArqTmp := fsDirECFLog + PathDelim + 'rfd.tmp' ;

  Ini := TMemIniFile.Create( ArqReducaoZ );
  SL  := TStringList.Create ;
  try
     with TACBrECF( fsECF ) do
     begin
        { Formantando campos do ECF e Contribuinte }
        wNumSerie := PadRight( fsECF_NumSerie, 20) ;
        wLetraMF  := ' ' ;
        if CharIsAlpha(fsECF_NumSerie[Length(fsECF_NumSerie)]) then
           wLetraMF := fsECF_NumSerie[Length(fsECF_NumSerie)] ;
        wModelo := '' ;
        wMarca  := AchaRFDID(fsECF_RFDID) ;
        P := pos('|',wMarca ) ;
        if P > 0 then
        begin
           wModelo := Trim( copy(wMarca ,P+1 ,Length(wMarca)) ) ;
           wMarca  := Trim( copy(wMarca ,1 ,P-1) ) ;
        end ;
        wMarca  := PadRight( wMarca,  20) ;
        wModelo := PadRight( wModelo, 20) ;

        wCNPJECF    := Poem_Zeros( fsCONT_CNPJ, 14) ;
        wNumUsuario := IntToStrZero( fsCONT_NumUsuario, 2) ;
        wNumCRZ     := Poem_Zeros( Ini.ReadString('ECF','NumCRZ','0'), 6) ;
        wDiaMov     := DtoS( DiaMov ) ;
        wTipoECF    := PadRight( cRFDTipoECF, 7) ;
        if fsECF_DataHoraSwBasico = 0 then
           wDtHrSwBasico := StringOfChar(' ',14)
        else
           wDtHrSwBasico := DTtoS( fsECF_DataHoraSwBasico ) ;

        { Cabecalho padrao para os registros de movimentação }
        CabOk  := wNumSerie + wLetraMF + wModelo + wNumUsuario ;
        CabNot := wNumSerie + wLetraMF +
                  StringReplace(wModelo,' ','?',[rfReplaceAll]) +
                  wNumUsuario ;

        { Gravando o registro E00 - Dados da Sw.House }
        Linha := 'E00' +
                 wNumSerie + wLetraMF +
                 wNumUsuario +
                 wTipoECF +
                 wMarca + wModelo +
                 Poem_Zeros( SH_COO, 6) +
                 Poem_Zeros( SH_NumeroAplicativo, 2) +
                 Poem_Zeros( SH_CNPJ, 14) +
                 Poem_Zeros( SH_IE  , 14) +
                 Poem_Zeros( SH_IM  , 14) +
                 PadRight( SH_RazaoSocial, 40) +
                 PadRight( SH_NomeAplicativo, 40) +
                 PadRight( SH_VersaoAplicativo, 10) +
                 PadRight( SH_Linha1, 42) +
                 PadRight( SH_Linha2, 42) ;
        WriteToTXT( ArqTmp, Linha, False );

        { Gravando o registro E01 - Identificação do ECF }
        Linha := 'E01' +
                 wNumSerie + wLetraMF +
                 wTipoECF +
                 wMarca + wModelo +
                 PadRight( NumVersao, 10) +
                 wDtHrSwBasico +
                 IntToStrZero( StrToIntDef( NumECF,1),3) +
                 wCNPJECF +
                 'ALT' +               // Metodo de geração
                 wNumCRZ + wNumCRZ +   // CRZ Inicial e Final
                 wDiaMov + wDiaMov +   // Data Inicial e Final
                 '01.00.00' +          // Verssao biblioteca
                 PadRight( fsAtoCotepe, 15) ;
        WriteToTXT( ArqTmp, Linha, True);
     end ;

     Linha := 'E02' +
              wNumSerie + wLetraMF +
              wModelo +
              wCNPJECF +
              PadRight( fsCONT_IE, 14) +
              PadRight( fsCONT_RazaoSocial, 40) +
              PadRight( fsCONT_Endereco, 120) +
              DTtoS( fsCONT_DataHoraCadastro ) +
              IntToStrZero( fsCONT_CROCadastro, 6) +
              IntToStrZero( Round(
                 Ini.ReadFloat('Totalizadores','GrandeTotal',0)*100), 18 ) + // IntToStrZero( Round( fsCONT_GTCadastro*100), 18 ) +
              wNumUsuario ;
     WriteToTXT( ArqTmp, Linha, True);

     { Gerando registro E12 - Relação de Redução Z }
     if FileExists( ArqReducaoZ ) then
     begin
        Linha := 'E12' + CabOk +
                 wNumCRZ +
                 Poem_Zeros( Ini.ReadString('ECF','NumCOO','0'), 6 ) +
                 Poem_Zeros( Ini.ReadString('ECF','NumCRO','0'), 6 ) +
                 wDiaMov +
                 Ini.ReadString('ECF','DataHoraEmissao',DTtoS(Now) ) +
                 IntToStrZero( Round(
                    Ini.ReadFloat('Totalizadores','VendaBruta',0)*100), 14 ) +
                 'N' ;  // Incidencia Desconto ISSQN ??
        WriteToTXT( ArqTmp, Linha, True);


        { Adicionando Totalizadores da Red.Z em um StringList }
        SL.Clear ;
        SL.Add('F1     '+IntToStrZero( Round(
           Ini.ReadFloat('OutrasICMS','TotalSubstituicaoTributaria',0)*100), 13 ) );
        SL.Add('I1     '+IntToStrZero( Round(
           Ini.ReadFloat('OutrasICMS','TotalIsencao',0)*100), 13 ) );
        SL.Add('N1     '+IntToStrZero( Round(
           Ini.ReadFloat('OutrasICMS','TotalNaoTributado',0)*100), 13 ) );
        SL.Add('AT     '+IntToStrZero( Round(
           Ini.ReadFloat('Totalizadores','TotalAcrescimos',0)*100), 13 ) );
        SL.Add('DT     '+IntToStrZero( Round(
           Ini.ReadFloat('Totalizadores','TotalDescontos',0)*100), 13 ) );
        SL.Add('Can-T  '+IntToStrZero( Round(
           Ini.ReadFloat('Totalizadores','TotalCancelamentos',0)*100), 13 ) );

        { Lendo toda a seção de aliquotas e adicionando no StringList }
        SEC := TStringList.Create ;
        try
           Ini.ReadSectionValues('Aliquotas',SEC);
           For I := 0 to SEC.Count-1 do
           begin
              P := pos('=',SEC[I]) ;
              if P > 0 then
              begin
                 SL.Add( IntToStrZero(I+1, 2) + PadRight(copy(SEC[I],3,P-1),5)+  // Alterado por Maicon da Silva Evangelista - Linha antiga-> SL.Add(PadRight(copy(SEC[I],1,P-1),7)+
                         IntToStrZero( Round( StrToFloatDef(
                                copy( SEC[I],P+1,Length(SEC[I]) ), 0)*100) , 13) );
              end ;
           end ;

           SL.Add( 'OPNF   '+IntToStrZero( Round(
                   Ini.ReadFloat('Totalizadores','TotalNaoFiscal',0)*100), 13 ) ) ;
        finally
           SEC.Free ;
        end ;

        { Ordenando e Gravando os Registros E13 }
        SL.Sort ;
        For I := 0 to SL.Count-1 do
        begin
           Linha := SL[I] ;
           if Linha <> '' then
           begin
              Linha  := 'E13' + CabOk + wNumCRZ + Linha ;
              WriteToTXT( ArqTmp, Linha, True);
           end ;
        end ;
     end ;


     { Processando registros E14 - Cupom Fiscal }
     Arq := fsDirECFLog + PathDelim + cRFDArqE14 ;
     if FileExists( Arq ) then
     begin
        SL.Clear ;
        SL.LoadFromFile( Arq );

        For I := 0 to SL.Count-1 do
        begin
           Linha  := SL[I] ;
           if Linha <> '' then
           begin
              HashOk := VerificaHashLinhaLog( Linha ) ;
              Linha  := 'E14' + IfThen(HashOk , CabOk, CabNot) + Linha ;
              WriteToTXT( ArqTmp, Linha, True);
           end ;
        end ;
     end ;

     { Processando registros E15 - Itens do Cupom Fiscal }
     Arq := fsDirECFLog + PathDelim + cRFDArqE15 ;
     if FileExists( Arq ) then
     begin
        SL.Clear ;
        SL.LoadFromFile( Arq );

        For I := 0 to SL.Count-1 do
        begin
           Linha  := SL[I] ;
           if Linha <> '' then
           begin
              HashOk := VerificaHashLinhaLog( Linha ) ;
              Linha  := 'E15' + IfThen(HashOk , CabOk, CabNot) + Linha ;
              WriteToTXT( ArqTmp, Linha, True);
           end ;
        end ;
     end ;

     { Processando registros E16 - Demais documentos emitidos pelo ECF }
     Arq := fsDirECFLog + PathDelim + cRFDArqE16 ;
     if FileExists( Arq ) then
     begin
        SL.Clear ;
        SL.LoadFromFile( Arq );

        For I := 0 to SL.Count-1 do
        begin
           Linha  := SL[I] ;
           if Linha <> '' then
           begin
              HashOk := VerificaHashLinhaLog( Linha ) ;
              Linha  := 'E16' + IfThen(HashOk , CabOk, CabNot) + Linha ;
              WriteToTXT( ArqTmp, Linha, True);
           end ;
        end ;
     end ;

     { Processando registros E21 - Pagamentos efetuados }
     Arq := fsDirECFLog + PathDelim + cRFDArqE21 ;
     if FileExists( Arq ) then
     begin
        SL.Clear ;
        SL.LoadFromFile( Arq );

        For I := 0 to SL.Count-1 do
        begin
           Linha  := SL[I] ;
           if Linha <> '' then
           begin
              HashOk := VerificaHashLinhaLog( Linha ) ;
              Linha  := 'E21' + IfThen(HashOk , CabOk, CabNot) + Linha ;
              WriteToTXT( ArqTmp, Linha, True);
           end ;
        end ;
     end ;
  finally
     SL.Free ;
     Ini.Free ;
  end ;

  { Assinando arquivo com registro EAD }
  if Assigned( fsOnCalcEAD ) then
     fsOnCalcEAD( ArqTmp )
  else
     FACBrEAD.AssinarArquivoComEAD( ArqTmp );

  if not DirectoryExists( DirECFMes ) then
     if not CreateDir( DirECFMes ) then
        raise EACBrRFDDirInvalido.Create( 'Erro ao criar o diretório: '+DirECFMes) ;

  CopyFileTo( ArqTmp, ArqRFD, False ) ;

  { Apagando os arquivos de LOG e Temporários }
  SysUtils.DeleteFile( ArqTmp ) ;
  SysUtils.DeleteFile( fsDirECFLog + PathDelim + cRFDArqE14 ) ;
  SysUtils.DeleteFile( fsDirECFLog + PathDelim + cRFDArqE15 ) ;
  SysUtils.DeleteFile( fsDirECFLog + PathDelim + cRFDArqE16 ) ;
  SysUtils.DeleteFile( fsDirECFLog + PathDelim + cRFDArqE21 ) ;
end;

function TACBrRFD.NomeArqRFD( DtMov : TDatetime ): String;
Var
  DirECFMes : String ;
begin
   DirECFMes := fsDirECF + PathDelim + FormatDateTime('yyyymm',DtMov) ;
   Result    := DirECFMes + PathDelim +
                NomeArqCAT52( fsECF_RFDID, fsECF_NumSerie, DtMov);
end;


procedure TACBrRFD.GravaLog(const Arq: String; Linha: String );
 Var Hash : String ;
begin
  Hash := CalcHashLinhaLog( Linha ) ;
  if Hash <> '' then
     Linha := Linha + '|$|' + Hash ;

  {$IFDEF LINUX}
    {$IFDEF FPC}
      FpUmask( 0 ) ;
    {$ELSE}
      umask( 0 ) ;
    {$ENDIF}
  {$ENDIF}
  WriteToTXT( fsDirECFLog + PathDelim + Arq , Linha ,
              True );  { True para adicionar no final do arquivo }
end;

Function TACBrRFD.CalcHashLinhaLog(Linha: String) : String ;
 Var Chave : String ;
begin
  Result := '' ;
  if Assigned( fsOnCalcHashLog ) then
     { Se usuário tem calculo próprio de Hash para Linha de LOG, então execute }
     fsOnCalcHashLog( Linha, Result )
  else
   begin
     { Calcula o CRC16 da Linha, adicionando "CHAVE"  para evitar fraudes
       (usuários editarem o arquivo de LOG manualmente e re-calcular o CRC16) }
     Chave := '' ;
     if Assigned( fsOnGetKeyHashLog ) then
        fsOnGetKeyHashLog( Chave ) ;   { Se usuário tem Chave própria, use-a }

     if Chave = '' then
        Chave := fsCONT_CNPJ ;

     Result := IntToStrZero( StringCrc16(Linha + Chave), 8) ;
   end ;
end ;

Function TACBrRFD.VerificaHashLinhaLog(var Linha: String) : Boolean ;
  Var P    : Integer ;
      HashLinha, HashCalc : String ;
begin
  Result := True ;
  P := Pos('|$|',Linha) ;
  if P > 0 then
  begin
     HashLinha := Trim(copy(Linha, P+3, Length(Linha))) ;
     Linha     := copy(Linha, 1, P-1) ;

     HashCalc  := Trim(CalcHashLinhaLog( Linha )) ;
     Result    := (HashLinha = HashCalc) ;
  end ;
end ;


procedure TACBrRFD.LerINI;
  Var Ini : TMemIniFile ;
      wDiaMov : TDateTime ;
begin
  if not FileExists(ArqINI) then
     raise Exception.Create(ACBrStr('Arquivo: '+ArqINI+' não encontrado.'));

  Ini := TMemIniFile.Create( ArqINI ) ;
  try
     ECF_CROAtual := Ini.ReadInteger('ECF','CROAtual', 0) ;
     ECF_RFDID    := Trim(Ini.ReadString('ECF','RFDID', '' )) ;
     try
        ECF_DataHoraSwBasico := StoD( Ini.ReadString('ECF', 'DataHoraSwBasico', '' ) ) ;
     except
        ECF_DataHoraSwBasico := 0 ;
     end ;
     try
        wDiaMov := StoD( Ini.ReadString('ECF','DiaMov', '') ) ;
     except
        {$IFDEF DELPHI10_UP}
         FileAge(ArqINI, wDiaMov) ;
        {$ELSE}
         wDiaMov := FileDateToDateTime( FileAge(ArqINI) ) ;
        {$ENDIF}
     end ;
     pDiaMov := wDiaMov ;

     AtoCotepe := Ini.ReadString('ECF', 'VersaoAtoCotepe', cRFDAtoCotepe ) ;

     CONT_RazaoSocial := Ini.ReadString('CONTRIBUINTE', 'Nome', '' ) ;
     CONT_CNPJ        := Ini.ReadString('CONTRIBUINTE', 'CNPJ', '' ) ;
     CONT_IE          := Ini.ReadString('CONTRIBUINTE', 'IE', '' ) ;
     CONT_Endereco    := Ini.ReadString('CONTRIBUINTE', 'Endereco', '' ) ;
     CONT_NumUsuario  := Ini.ReadInteger('CONTRIBUINTE', 'NumUsuario', -1 ) ;
     try
        CONT_DataHoraCadastro := StoD( Ini.ReadString('CONTRIBUINTE', 'DataHoraCadastro', '' ) ) ;
     except
        CONT_DataHoraCadastro := 0 ;
     end ;
     CONT_CROCadastro := Ini.ReadInteger('CONTRIBUINTE', 'CROCadastro', -1 ) ;
     CONT_GTCadastro  := Ini.ReadFloat('CONTRIBUINTE', 'GTCadastro', -1 ) ;
  finally
     Ini.Free ;
  end ;
end;

procedure TACBrRFD.GravarINI;
  Var Ini : TMemIniFile ;
begin
  Ini := TMemIniFile.Create( ArqINI ) ;
  try
     Ini.WriteString('ECF','DiaMov', DtoS(DiaMov) ) ;
     Ini.WriteInteger('ECF','CROAtual', fsECF_CROAtual ) ;
     Ini.WriteString('ECF','RFDID', fsECF_RFDID+' ' ) ;
     Ini.WriteString('ECF', 'VersaoAtoCotepe', fsAtoCotepe ) ;
     if fsCONT_DataHoraCadastro = 0 then
        Ini.WriteString('ECF', 'DataHoraSwBasico', 'YYYYMMDDhhmmss' )
     else
        Ini.WriteString('ECF', 'DataHoraSwBasico', DTtoS(fsECF_DataHoraSwBasico) ) ;

     Ini.WriteString('CONTRIBUINTE', 'Nome', fsCONT_RazaoSocial+' ' ) ;
     Ini.WriteString('CONTRIBUINTE', 'CNPJ', fsCONT_CNPJ+' ' ) ;
     Ini.WriteString('CONTRIBUINTE', 'IE', fsCONT_IE+' ' ) ;
     Ini.WriteString('CONTRIBUINTE', 'Endereco', fsCONT_Endereco+' ' ) ;
     Ini.WriteInteger('CONTRIBUINTE', 'NumUsuario', fsCONT_NumUsuario ) ;
     if fsCONT_DataHoraCadastro = 0 then
        Ini.WriteString('CONTRIBUINTE', 'DataHoraCadastro', 'YYYYMMDDhhmmss' )
     else
        Ini.WriteString('CONTRIBUINTE', 'DataHoraCadastro', DTtoS(fsCONT_DataHoraCadastro) ) ;
     Ini.WriteInteger('CONTRIBUINTE', 'CROCadastro', fsCONT_CROCadastro ) ;
     Ini.WriteFloat('CONTRIBUINTE', 'GTCadastro', fsCONT_GTCadastro ) ;

  finally
     Ini.UpdateFile ;
     Ini.Free ;
  end ;
end;


procedure TACBrRFD.SetSH_RazaoSocial(const AValue: String);
begin
  fsSH_RazaoSocial := LeftStr(AValue,40) ;
end;

procedure TACBrRFD.SetSH_CNPJ(const AValue: String);
begin
  fsSH_CNPJ := Trim( OnlyNumber( AValue ) ) ;
end;

procedure TACBrRFD.SetSH_IE(const AValue: String);
begin
  fsSH_IE := Trim( OnlyNumber( AValue ) ) ;
end;

procedure TACBrRFD.SetSH_IM(const AValue: String);
begin
  fsSH_IM := Trim( OnlyNumber( AValue ) ) ;
end;

procedure TACBrRFD.SetSH_COO(const AValue: String);
begin
  fsSH_COO := Trim( OnlyNumber( AValue ) ) ;
end;

procedure TACBrRFD.SetSH_Linha1(const AValue: String);
begin
  fsSH_Linha1 := LeftStr( AValue, 42);
end;

procedure TACBrRFD.SetSH_Linha2(const AValue: String);
begin
  fsSH_Linha2 := LeftStr( AValue, 42);
end;

procedure TACBrRFD.SetSH_NomeAplicativo(const AValue: String);
begin
  fsSH_NomeAplicativo := LeftStr( AValue, 40);
end;

procedure TACBrRFD.SetSH_NumeroAplicativo(const AValue: String);
begin
  fsSH_NumeroAplicativo := LeftStr( AValue, 2);
end;

procedure TACBrRFD.SetSH_VersaoAplicativo(const AValue: String);
begin
  fsSH_VersaoAplicativo := LeftStr( AValue, 10);
end;

procedure TACBrRFD.SetAtoCotepe(const AValue: String);
begin
  fsAtoCotepe := LeftStr(AValue, 15);
end;

procedure TACBrRFD.SetECF_RFDID(const AValue: String);
begin
  fsECF_RFDID := LeftStr( UpperCase(AValue), 3);
end;

procedure TACBrRFD.SetOnRFDGetKeyRSA(const AValue: TACBrEADGetChave);
begin
  FACBrEAD.OnGetChavePrivada := AValue;
end;

procedure TACBrRFD.SetCONT_Endereco(const AValue: String);
begin
  fsCONT_Endereco := Trim(LeftStr(AValue,120));
end;

procedure TACBrRFD.SetCONT_CNPJ(const AValue: String);
begin
  fsCONT_CNPJ := Trim(LeftStr( OnlyNumber( AValue ), 14));
end;

procedure TACBrRFD.SetCONT_IE(const AValue: String);
begin
  fsCONT_IE := Trim(LeftStr( OnlyNumber( AValue ), 14));
end;

procedure TACBrRFD.SetCONT_RazaoSocial(const AValue: String);
begin
  fsCONT_RazaoSocial := Trim(LeftStr( AValue, 40));
end;



procedure TACBrRFD.SetECF(const AValue: TACBrComponent);
  Var OldValue : TACBrECF ;
begin
  if AValue <> fsECF then
  begin
     if AValue <> nil then
        if not (AValue is TACBrECF) then
           raise Exception.Create(ACBrStr('ACBrRFD.ECF deve ser do tipo TACBrECF')) ;

     if Assigned(fsECF) then
        fsECF.RemoveFreeNotification(Self);

     OldValue := TACBrECF(fsECF) ;   // Usa outra variavel para evitar Loop Infinito
     fsECF := AValue;                 // na remoção da associação dos componentes

     if Assigned(OldValue) then
        if Assigned(OldValue.ECF) then
           OldValue.RFD := nil ;

     if AValue <> nil then
     begin
        AValue.FreeNotification(self);
        TACBrECF(AValue).RFD := self ;
     end ;
  end ;
end;

procedure TACBrRFD.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (fsECF <> nil) and (AComponent is TACBrECF) then
     fsECF := nil ;
end;

function TACBrRFD.AchaRFDID(RFDID: String): String;
 Var Ini : TMemIniFile ;
     Modelo : String ;
begin
  Result := '' ;
  RFDID  := Trim(RFDID) ;
  if (ArqRFDID = '') or (length(RFDID) < 2) then
     exit ;

  if not FileExists( ArqRFDID ) then
     CriarArqRFDID( ArqRFDID ) ;

  Ini := TMemIniFile.Create( ArqRFDID ) ;
  try
     Modelo := '' ;
     Result := Ini.ReadString('Marcas',LeftStr(RFDID,2),'') ;

     if Length( RFDID ) > 2 then
     begin
        Modelo := Ini.ReadString('Modelos',LeftStr(RFDID,3),'') ;
        if Modelo <> '' then
           Result := Result + ' | '+Modelo ;
     end ;
  finally
     Ini.Free ;
  end ;
end;

procedure TACBrRFD.CriarArqRFDID( const NomeArq : String ) ;
Var
  SL : TStringList ;
begin
  if NomeArq = '' then exit ;

  if not FileExists( NomeArq ) then
  begin
     SL := TStringList.Create ;
     try
        SL.Clear ;
        SL.Add('[Marcas]') ;
        SL.Add('AO=AOKI') ;
        SL.Add('AS=ASTICK') ;
        SL.Add('BE=BEMATECH') ;
        SL.Add('BT=BETHA') ;
        SL.Add('CH=CHRONOS') ;
        SL.Add('CO=CORISCO') ;
        SL.Add('DR=DARUMA AUTOMACAO') ;
        SL.Add('DT=DATAREGIS') ;
        SL.Add('DA=DIGIARTE') ;
        SL.Add('DS=DIGISAT') ;
        SL.Add('DI=DISMAC') ;
        SL.Add('EA=EAGLE') ;
        SL.Add('EL=ELGIN') ;
        SL.Add('EP=EPSON') ;
        SL.Add('GE=GENERAL') ;
        SL.Add('IB=IBM') ;
        SL.Add('IC=ICASH') ;
        SL.Add('IO=IONICS') ;
        SL.Add('IP=ITAUTEC') ;
        SL.Add('ME=MECAF') ;
        SL.Add('NC=NCR') ;
        SL.Add('PE=PERTO') ;
        SL.Add('PR=PROCOMP') ;
        SL.Add('QL=QUALID') ;
        SL.Add('QT=QUATRO') ;
        SL.Add('RB=ROBOMARKET') ;
        SL.Add('RR=ROR') ;
        SL.Add('SC=SCHALTER') ;
        SL.Add('SD=SID') ;
        SL.Add('SG=SIGTRON') ;
        SL.Add('SI=SONDA') ;
        SL.Add('SW=SWEDA') ;
        SL.Add('TP=TERMOPRINTER') ;
        SL.Add('TR=TRENDS') ;
        SL.Add('UG=UNIGRAPH') ;
        SL.Add('UN=UNISYS') ;
        SL.Add('UR=URANO') ;
        SL.Add('YA=YANCO') ;
        SL.Add('ZT=ZANTHUS') ;
        SL.Add('ZP=ZPM') ;
        SL.Add('') ;
        SL.Add('[Modelos]') ;
        SL.Add('AO1=AOKI 1E') ;
        SL.Add('AS1=ASTICK 100') ;
        SL.Add('AS2=ASTICK L') ;
        SL.Add('BE1=ECF-IF MP-20 FI') ;
        SL.Add('BE2=ECF-IF MP-20 FI R') ;
        SL.Add('BE3=ECF-IF MP-40 FI') ;
        SL.Add('BE4=KIT ECF-IF MP-30 FI') ;
        SL.Add('BE5=MP-20 FI DUAL ECF-IF') ;
        SL.Add('BE6=MP-20 FI II ECF-IF') ;
        SL.Add('BE7=MP-20 FI II R ECF-IF') ;
        SL.Add('BE8=MP-40 FI II ECF-IF') ;
        SL.Add('BE9=MP-40 FI II R ECF-IF') ;
        SL.Add('BEA=MP/20 FI II ECF-IF') ;
        SL.Add('BEB=MP-25 FI') ;
        SL.Add('BEC=MP-50 FI') ;
        SL.Add('BED=MP-2000 TH FI') ;
        SL.Add('BEE=MP-2100 TH FI') ;
        SL.Add('BEF=MP-3000 TH FI') ;
        SL.Add('BEG=MP-6000 TH FI') ;
        SL.Add('BEH=MP-6100 TH FI') ;
        SL.Add('BEI=MP-7000 TH FI') ;
        SL.Add('BEJ=MP-4000 TH FI') ;

        SL.Add('BT1=BETHA 2E') ;
        SL.Add('CH1=CHRONOS-250 1E') ;
        SL.Add('CH2=CHRONOS-270 2E') ;
        SL.Add('CH3=MULTI-50110') ;
        SL.Add('CO1=ECF-IF CT7000-V1') ;
        SL.Add('CO2=ECF-IF CT7000V3') ;
        SL.Add('DR1=ECF-IF FS 500') ;
        SL.Add('DR2=ECF-IF FS2000') ;
        SL.Add('DR3=FS-318') ;
        SL.Add('DR4=FS-345') ;
        SL.Add('DR5=PRINT PLUS FS-335') ;
        SL.Add('DT1=1Q') ;
        SL.Add('DT2=300-EP') ;
        SL.Add('DT3=375-EP') ;
        SL.Add('DT4=950-EP') ;
        SL.Add('DT5=DT-4000') ;
        SL.Add('DT6=IF/1') ;
        SL.Add('DT7=IF/1N') ;
        SL.Add('DT8=IF/2') ;
        SL.Add('DA1=DIGIARTE 1') ;
        SL.Add('DA2=DIGIARTE 1E') ;
        SL.Add('DS1=1E') ;
        SL.Add('DI1=2001-II') ;
        SL.Add('EA1=PRINTER 2000 ECF-IF') ;
        SL.Add('EA2=PRINTER 2000 II ECF-IF') ;
        SL.Add('EA3=PRINTER 2000 II R ECF-IF') ;
        SL.Add('EA4=PRINTER 2001 ECF-IF') ;
        SL.Add('EA5=PRINTER 2002 II') ;
        SL.Add('EA6=PRINTER 2002 II ECF-IF') ;
        SL.Add('EL1=ECF IF 400 2E') ;
        SL.Add('EL2=ECF IF 500 1E') ;
        SL.Add('EL3=ECF IF 400 1E-EP') ;
        SL.Add('EL4=ECF-IF 600-2E-OL') ;
        SL.Add('EL5=ECF-MR 10000-S') ;
        SL.Add('EL6=ECF-MR 10000-S1') ;
        SL.Add('EL7=ECF-MR 12000-S') ;
        SL.Add('EL8=ECF-MR 800-S') ;
        SL.Add('EL9=ECF-MR 800S2') ;
        SL.Add('ELA=ELGIN FIT') ;
        SL.Add('ELB=FX7') ;
        SL.Add('ELC=IF 6000TH') ;
        SL.Add('ELD=X5') ;
        SL.Add('EP1=TM-H6000 FB');
        SL.Add('EP2=TM-H6000 FBII');
        SL.Add('EP3=TM-T81 FBII');
        SL.Add('EP4=TM-T88 FB');
        SL.Add('EP5=TM-T88 FBII');  
        SL.Add('GE1=ECF-IF GP-2000') ;
        SL.Add('IB1=4679 3BM') ;
        SL.Add('IB2=4679 3BS') ;
        SL.Add('IB3=ECF-IF 4679-3FB') ;
        SL.Add('IB4=IB 20 FI II ECF-IF') ;
        SL.Add('IB5=IB 40 FI II ECF-IF') ;
        SL.Add('IB6=IB-20 FI II R ECF-IF') ;
        SL.Add('IC1=IF - II') ;
        SL.Add('IO1=IONICS 1E') ;
        SL.Add('IP1=KIT ECF-IF/1E') ;
        SL.Add('IP2=KIT POS 4000 ECF-IF/1E') ;
        SL.Add('IP3=KIT POS 4000 ECF-IF/3E') ;
        SL.Add('IP4=POS 4000 1E') ;
        SL.Add('IP5=POS 4000 3E') ;
        SL.Add('IP6=POS 4000 ECF-IF/1E') ;
        SL.Add('IP7=POS 4000 ECF-IF/1E BR') ;
        SL.Add('IP8=POS 4000 ECF-IF/3E') ;
        SL.Add('IP9=POS 4000 ECF-IF/3E BR') ;
        SL.Add('IPA=POS ECF IF/2E M') ;
        SL.Add('IPB=POS4000 ECF-IF/1E II') ;
        SL.Add('IPC=POS4000 ECF-IF/3E II') ;
        SL.Add('ME1=ECF 1E-3001') ;
        SL.Add('ME2=ECF-4002') ;
        SL.Add('ME3=MECAF COMPACT FCR') ;
        SL.Add('NC1=ECF-IF 72EPS-01') ;
        SL.Add('NC2=ECF-IF 72EPS-02') ;
        SL.Add('NC3=ECF-IF-02-01') ;
        SL.Add('NC4=ECF-IF-03-02') ;
        SL.Add('NC5=ECF-IF-7141') ;
        SL.Add('NC6=ECF-IF-7424E2i') ;
        SL.Add('PR1=ECF 2001') ;
        SL.Add('PR2=ECF 2002') ;
        SL.Add('PR3=ECF 2011') ;
        SL.Add('PR4=ECF 2023') ;
        SL.Add('QL1=CASH TOP ECF-IF') ;
        SL.Add('QT1=EASY APF') ;
        SL.Add('QT2=EASY IIF') ;
        SL.Add('QT3=ECF-IF EASY APF') ;
        SL.Add('RB1=RM 1') ;
        SL.Add('RR1=IF ROR 1E') ;
        SL.Add('SC1=D PRINT') ;
        SL.Add('SC2=D PRINT ECF') ;
        SL.Add('SC3=ECF IF SCFI 1E') ;
        SL.Add('SC4=S PRINT') ;
        SL.Add('SC5=T PRINT') ;
        SL.Add('SC6=T PRINT-ECF') ;
        SL.Add('SD1=6417') ;
        SL.Add('SD2=SID 6459') ;
        SL.Add('SD3=SID 6460') ;
        SL.Add('SG1=FS-318') ;
        SL.Add('SG2=FS-345') ;
        SL.Add('SG3=FS367') ;
        SL.Add('SG4=PRINT PLUS- FS 210') ;
        SL.Add('SG5=PRINT PLUS-FS 100') ;
        SL.Add('SG6=PRINT PLUS-FS 200') ;
        SL.Add('SG7=PRINT PLUS-FS 200 G') ;
        SL.Add('SG8=PRINT PLUS-FS 215') ;
        SL.Add('SG9=PRINT PLUS-FS 220') ;
        SL.Add('SGA=PRINT PLUS-FS 300') ;
        SL.Add('SGB=PRINT PLUS-FS 315') ;
        SL.Add('SGC=PRINT PLUS-FS 320') ;
        SL.Add('SGD=PRINT PLUS-FS 335') ;
        SL.Add('SGE=PRINT PLUS-FS 350') ;
        SL.Add('SGF=PRINT PLUS-FS 365') ;
        SL.Add('SGG=PRINT PLUS-FS 370') ;
        SL.Add('SGH=PRINT PLUS-FS 395') ;
        SL.Add('SW1=IF S-7000 III') ;
        SL.Add('SW2=IF S-7000I') ;
        SL.Add('SW3=IF S-7000IE') ;
        SL.Add('SW4=IF S-7000II') ;
        SL.Add('SW5=IF S-9000I') ;
        SL.Add('SW6=IF S-9000IE') ;
        SL.Add('SW7=IF S-9000II') ;
        SL.Add('SW8=IF S-9000IIE') ;
        SL.Add('SW9=IF S-9000IIIE') ;

        //===================================//
        SL.Add('SWA=ST100');
        SL.Add('SWB=ST1000');
        SL.Add('SWC=ST200');
        SL.Add('SWD=ST120');
        SL.Add('SWE=ST2000');
        SL.Add('SWF=ST2500');
        //===================================//

        SL.Add('TR1=TRENDS 1.0 E') ;
        SL.Add('UG1=UN-FI') ;
        SL.Add('UG2=UN-FI 2E') ;
        SL.Add('UN1=BR 1002-ECF') ;
        SL.Add('UN2=BR-20 IF2 ECF-IF') ;
        SL.Add('UN3=BR-40 IF2 ECF-IF') ;
        SL.Add('UN4=BR400-IF ECF-IF') ;
        SL.Add('UN5=BRB 375 ECF-IF') ;
        SL.Add('UN6=ECF-IF BR401-IF') ;
        SL.Add('UR1=ECF-IF URANO/1EFREST') ;
        SL.Add('UR2=ECF-IF URANO/2EFCR') ;
        SL.Add('UR3=KIT URANO/2EFC') ;
        SL.Add('UR4=URANO/1EFC') ;
        SL.Add('UR5=URANO/2EFC') ;
        SL.Add('UR6=URANO/2EFE') ;
        SL.Add('UR7=ZPM/1EF') ;
        SL.Add('YA1=ECF-IF YANCO 8000') ;
        SL.Add('YA2=ECF-IF YANCO8500') ;
        SL.Add('ZT1=1e') ;
        SL.Add('ZT2=2E-ECF') ;
        SL.Add('ZT3=ECF-IF QZ1000') ;
        SL.Add('ZT4=IZ 11-ECF') ;
        SL.Add('ZT5=IZ 21-ECF') ;
        SL.Add('ZT6=IZ10-ECF') ;
        SL.Add('ZT7=IZ20-ECF') ;
        SL.Add('ZT8=IZ22') ;
        SL.Add('ZT9=IZ41-ECF') ;
        SL.Add('ZTA=IZ51') ;
        SL.Add('ZTB=QZ 1001') ;
        SL.Add('ZTC=QZ1000') ;
        SL.Add('ZTD=QZ2000') ;
        SL.Add('ZP1=ZPM/1EF') ;
        SL.Add('ZP2=ZPM/2EFC') ;

        SL.SaveToFile( NomeArq );
     finally
        SL.Free ;
     end ;
  end ;
end;

end.

