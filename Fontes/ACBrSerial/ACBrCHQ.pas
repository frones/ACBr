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
|* 20/08/2004: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrCHQ
|* 23/01/2007: Daniel Simoes de Almeida
|*  - Corrigido Problemas de FreeNotification com a propriedade "ECF"
******************************************************************************}

{$I ACBr.inc}

unit ACBrCHQ;

interface
uses ACBrBase, ACBrDevice, ACBrCHQClass, ACBrECF,  { Units da ACBr}
     SysUtils , Classes;

type
  TACBrCHQModelo = (chqNenhuma, chqImpressoraECF, chqImpressoraComum,
                    chqBematech, chqChronos, chqSchalter, chqPerto,
                    chqSotomaq, chqUrano, chqElgin) ;

{ Componente ACBrCHQ }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrCHQ = class( TACBrComponent )
  private
    fsDevice : TACBrDevice ;   { SubComponente ACBrDevice }

    { Propriedades do Componente ACBrCHQ }
    fsAtivo  : Boolean;
    fsModelo : TACBrCHQModelo;
    fsCHQ    : TACBrCHQClass ;   { Classe com instancia da Gaveta de fsModelo }
    fsECF    : TACBrECF;
    fsModelosCheque: TACBrCHQModelos;

    procedure SetModelo(const Value: TACBrCHQModelo);
    procedure SetPorta(const Value: String);
    procedure SetAtivo(const Value: Boolean);
    procedure SetPaginaDeCodigoClass(const Value: Word);

    function GetPorta: String;
    function GetModeloStrClass: String;
    function GetPaginaDeCodigoClass: Word;
    procedure SetECF(const Value: TACBrECF);
    function GetValorClass: Double;
    procedure SetValorClass(const Value: Double);
    function GetBancoClass: String;
    function GetCidadeClass: String;
    function GetDataClass: TDateTime;
    function GetFavorecidoClass: String;
    procedure SetBancoClass(const Value: String);
    procedure SetCidadeClass(const Value: String);
    procedure SetDataClass(const Value: TDateTime);
    procedure SetFavorecidoClass(const Value: String);
    function GetObservacaoClass: String;
    procedure SetObservacaoClass(const Value: String);
    function GetChequeProntoClass: Boolean;

    function GetArquivoBemaFiINI : String;
    procedure SetArquivoBemaFiINI(const Value: String);

    function GetCMC7Class : String;
    function GetBomParaClass : TDatetime;
    procedure SetBomParaClass(const Value: TDateTime);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy  ; override ;

    procedure Ativar ;
    procedure Desativar ;
    property Ativo : Boolean read fsAtivo write SetAtivo ;

    property ModelosCheque : TACBrCHQModelos read fsModelosCheque
       write fsModelosCheque ;

    property CHQ : TACBrCHQClass read fsCHQ ;
    Property ModeloStr : String read GetModeloStrClass;

    procedure ImprimirCheque ;
    Procedure TravarCheque ;
    Procedure DestravarCheque ;
    procedure ImprimirLinha( const AString : AnsiString ) ;
    procedure ImprimirVerso( AStringList : TStrings ) ; Virtual ;

    property ChequePronto : Boolean read GetChequeProntoClass ;

    property CMC7 : String read GetCMC7Class;
    property BomPara : TDateTime read GetBomParaClass write SetBomParaClass;

  published
     property Modelo : TACBrCHQModelo read fsModelo write SetModelo
                 default chqNenhuma ;
     property Porta : String read GetPorta write SetPorta ;

     // Pagina de Código
     property PaginaDeCodigo: Word   read GetPaginaDeCodigoClass write SetPaginaDeCodigoClass  default 0;

     { Propriedades para preencher o Cheque }
     property Banco : String read GetBancoClass write SetBancoClass
        stored false;
     property Valor : Double read GetValorClass write SetValorClass
        stored false;
     property Data : TDateTime read GetDataClass write SetDataClass
        stored false;
     property Cidade : String read GetCidadeClass write SetCidadeClass ;
     property Favorecido: String read GetFavorecidoClass
        write SetFavorecidoClass ;
     property Observacao : String read GetObservacaoClass
        write SetObservacaoClass ;

     property ArquivoBemaFiINI : String read GetArquivoBemaFiINI
        write SetArquivoBemaFiINI ;

     { Instancia do Componente ACBrDevice, será passada para fsCHQ.create }
     property Device : TACBrDevice read fsDevice ;

     property ECF    : TACBrECF    read fsECF write SetECF ;
end ;


implementation
Uses ACBrUtil, ACBrCHQImpressoraECF, ACBrCHQImpressoraComum, ACBrCHQBematech,
     ACBrCHQChronos, ACBrCHQSchalter, ACBrCHQPerto, ACBrCHQSotomaq,
     ACBrCHQUrano, ACBrCHQElgin;

{ TACBrCHQ }

constructor TACBrCHQ.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  fsAtivo  := false ;
  fsModelo := chqNenhuma ;

  { Instanciando SubComponente TACBrDevice }
  fsDevice := TACBrDevice.Create( self ) ;  { O dono é o proprio componente }
  fsDevice.Name := 'ACBrDevice' ;    { Apenas para aparecer no Object Inspector}
  {$IFDEF COMPILER6_UP}
  fsDevice.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}
  fsDevice.Porta := 'COM1';
  fsDevice.TimeOut := 1 ;

  { Instanciando fsGAV com modelo Generico (GAVClass) }
  fsCHQ := TACBrCHQClass.create( self ) ;

  { Carregando tabela de Modelos de Cheque }
  fsModelosCheque := TACBrCHQModelos.Create( true );
end;

destructor TACBrCHQ.Destroy;
begin
  Ativo := false ;

  if Assigned( fsCHQ ) then
     FreeAndNil( fsCHQ ) ;

  FreeAndNil( fsDevice ) ;

  if Assigned( fsModelosCheque ) then
     fsModelosCheque.Free ;

  inherited Destroy;
end;


procedure TACBrCHQ.SetModelo(const Value: TACBrCHQModelo);
Var wBanco      : String ;
    wValor      : Double ;
    wData       : TDateTime ;
    wCidade     : String ;
    wFavorecido : String ;
    wObservacao : String ;
    wPaginaCod  : Word ;
begin
  if fsModelo = Value then exit ;

  if fsAtivo then
     raise Exception.Create(ACBrStr('Não é possível mudar o Modelo com ACBrCHQ Ativo'));

  wBanco      := Banco ;
  wValor      := Valor ;
  wData       := Data;
  wCidade     := Cidade ;
  wFavorecido := Favorecido ;
  wObservacao := Observacao ;
  wPaginaCod  := PaginaDeCodigo ;

  FreeAndNil( fsCHQ ) ;

  { Instanciando uma nova classe de acordo com fsModelo }
  case Value of
     chqBematech        : fsCHQ := TACBrCHQBematech.create( Self ) ;
     chqChronos         : fsCHQ := TACBrCHQChronos.create( Self ) ;
     chqSchalter        : fsCHQ := TACBrCHQSchalter.create( Self ) ;
     chqPerto           : fsCHQ := TACBrCHQPerto.create( Self ) ;
     chqSotomaq         : fsCHQ := TACBrCHQSotomaq.create( Self ) ;
     chqUrano           : fsCHQ := TACBrCHQUrano.create( Self ) ;
     chqImpressoraECF   : fsCHQ := TACBrCHQImpressoraECF.create( Self ) ;
     chqImpressoraComum : fsCHQ := TACBrCHQImpressoraComum.create( Self ) ;
     chqElgin           : fsCHQ := TACBrCHQElgin.Create(Self) ;
  else
     fsCHQ := TACBrCHQClass.create( Self ) ;
  end;

  Banco      := wBanco ;
  Valor      := wValor ;
  Data       := wData;
  Cidade     := wCidade ;
  Favorecido := wFavorecido ;
  Observacao := wObservacao ;
  PaginaDeCodigo := wPaginaCod ;

  fsModelo := Value;

  if Value <> chqImpressoraECF then
     ECF := nil
  else
     Porta := '' ;
end;

procedure TACBrCHQ.SetAtivo(const Value: Boolean);
begin
  if Value then
     Ativar
  else
     Desativar ;
end;

procedure TACBrCHQ.Ativar;
begin
  if fsAtivo then exit ;

  if fsModelo = chqNenhuma then
     raise Exception.Create(ACBrStr('Modelo não definido'));

  fsCHQ.Ativar ;

  fsAtivo := true ;
end;

procedure TACBrCHQ.Desativar;
begin
  if not fsAtivo then exit ;

  fsCHQ.Desativar ;
  fsAtivo := false;
end;

procedure TACBrCHQ.SetPorta(const Value: String);
begin
  if Modelo in [chqImpressoraECF] then
     fsDevice.Porta := ''
  else
     fsDevice.Porta := Value ;
end;

procedure TACBrCHQ.ImprimirCheque ;
Var wAtivo : Boolean ;
begin
  wAtivo := Ativo ;
  try
     Ativo := true ;
     fsCHQ.ImprimirCheque ;
  finally
     Ativo := wAtivo ;
  end ;
end;

procedure TACBrCHQ.ImprimirLinha( const AString : AnsiString );
Var wAtivo : Boolean ;
begin
  wAtivo := Ativo ;
  try
     Ativo := true ;
     fsCHQ.ImprimirLinha( AString ) ;
  finally
     Ativo := wAtivo ;
  end ;
end;

procedure TACBrCHQ.ImprimirVerso(AStringList: TStrings );
Var wAtivo : Boolean ;
begin
  wAtivo := Ativo ;
  try
     Ativo := true ;
     fsCHQ.ImprimirVerso( AStringList ) ;
  finally
     Ativo := wAtivo ;
  end ;
end;

procedure TACBrCHQ.DestravarCheque;
Var wAtivo : Boolean ;
begin
  wAtivo := Ativo ;
  try
     Ativo := true ;
     fsCHQ.DestravarCheque ;
  finally
     Ativo := wAtivo ;
  end ;
end;

procedure TACBrCHQ.TravarCheque;
Var wAtivo : Boolean ;
begin
  wAtivo := Ativo ;
  try
     Ativo := true ;
     fsCHQ.TravarCheque ;
  finally
     Ativo := wAtivo ;
  end ;
end;

function TACBrCHQ.GetCMC7Class : String;
begin
  Result := fsCHQ.CMC7;
end;

function TACBrCHQ.GetBomParaClass : TDateTime;
begin
  Result := fsCHQ.BomPara;
end;

procedure TACBrCHQ.SetBomParaClass(const Value: TDateTime);
begin
  fsCHQ.BomPara := Value;
end;

function TACBrCHQ.GetModeloStrClass: String;
begin
  Result := ACBrStr(fsCHQ.ModeloStr) ;
end;

function TACBrCHQ.GetPorta: String;
begin
  Result := fsDevice.Porta ;
end;

function TACBrCHQ.GetChequeProntoClass: Boolean;
Var wAtivo : Boolean ;
begin
  wAtivo := Ativo ;
  Ativo  := true ;

  Result := fsCHQ.ChequePronto ;

  Ativo := wAtivo ;
end;

function TACBrCHQ.GetBancoClass: String;
begin
  Result := fsCHQ.Banco ;
end;

function TACBrCHQ.GetDataClass: TDateTime;
begin
  Result := fsCHQ.Data ;
end;

function TACBrCHQ.GetCidadeClass: String;
begin
  Result := fsCHQ.Cidade ;
end;

function TACBrCHQ.GetFavorecidoClass: String;
begin
  Result := fsCHQ.Favorecido ;
end;

function TACBrCHQ.GetObservacaoClass: String;
begin
  Result := fsCHQ.Observacao ;
end;

function TACBrCHQ.GetArquivoBemaFiINI : String;
begin
  Result := fsModelosCheque.ArquivoBemaFiINI ;
end;

function TACBrCHQ.GetValorClass: Double;
begin
  Result := fsCHQ.Valor ;
end;

procedure TACBrCHQ.SetBancoClass(const Value: String);
begin
  fsCHQ.Banco := Value ;
end;

procedure TACBrCHQ.SetValorClass(const Value: Double);
begin
  fsCHQ.Valor := Value ;
end;

procedure TACBrCHQ.SetDataClass(const Value: TDateTime);
begin
  fsCHQ.Data := Value ;
end;

procedure TACBrCHQ.SetFavorecidoClass(const Value: String);
begin
  fsCHQ.Favorecido := Value ;
end;

procedure TACBrCHQ.SetCidadeClass(const Value: String);
begin
  fsCHQ.Cidade := Value ;
end;

procedure TACBrCHQ.SetObservacaoClass(const Value: String);
begin
  fsCHQ.Observacao := Value ;
end;

procedure TACBrCHQ.SetArquivoBemaFiINI(const Value: String);
begin
  fsModelosCheque.ArquivoBemaFiINI := Value ;
end;

procedure TACBrCHQ.SetECF(const Value: TACBrECF);
begin
  if Value <> fsECF then
  begin
     if Assigned(fsECF) then
        fsECF.RemoveFreeNotification(Self);

     fsECF     := Value;
     fsCHQ.ECF := Value ;

     if fsECF is TACBrECF then
        Modelo := chqImpressoraECF ;

     if Value <> nil then
        Value.FreeNotification(self);
  end ;
end;

procedure TACBrCHQ.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (fsCHQ <> nil) and (AComponent is TACBrECF) then
  begin
     fsECF := nil ;

     if (fsCHQ.ECF <> nil) then
        fsCHQ.ECF := nil ;
  end ;
end;

procedure TACBrCHQ.SetPaginaDeCodigoClass(const Value: Word);
begin
  fsCHQ.PaginaDeCodigo := Value ;
end;

function TACBrCHQ.GetPaginaDeCodigoClass: Word;
begin
  Result := fsCHQ.PaginaDeCodigo;
end;


end.
