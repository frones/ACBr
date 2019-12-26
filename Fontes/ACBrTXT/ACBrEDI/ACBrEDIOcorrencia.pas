{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2019 Daniel Simoes de Almeida               }
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
unit ACBrEDIOcorrencia;

{$I ACBr.inc}

interface

uses SysUtils, Classes, ACBrBase,
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
    ACBrEDIClass, pediConversao, ACBrTxtClass, Contnrs ;

type

	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}

   TInfoCompl = class // Usado nas Versão 5.0 Registro 543
   private
     FIdRegistro: String ;
     FxMotivo1  : String ;
     FxMotivo2  : String ;
     FxMotivo3  : String ;
     FFiller    : String ;
   public
     property IdRegistro: String  read FIdRegistro  write FIdRegistro ;
     property xMotivo1  : String  read FxMotivo1    write FxMotivo1 ;
     property xMotivo2  : String  read FxMotivo2    write FxMotivo2 ;
     property xMotivo3  : String  read FxMotivo3    write FxMotivo3 ;
     property Filler    : String  read FFiller      write FFiller ;
   end;

   TItemNF = class  // Usado EDI 5.0  Registro 544
   private
     FIdRegistro: String ;
     FcItem     : String ;
     FxDescricao: String ;
     FqVolumeNF : Double ;
     FqVolEntreg: Double ;
     FFiller    : String ;
   public
     property IdRegistro: String  read FIdRegistro  write FIdRegistro ;
     property cItem     : String  read FcItem       write FcItem ;
     property xDescricao: String  read FxDescricao  write FxDescricao ;
     property qVolumeNF : Double  read FqVolumeNF   write FqVolumeNF ;
     property qVolEntreg: Double  read FqVolEntreg  write FqVolEntreg ;
     property Filler    : String  read FFiller      write FFiller ;
   end;

   TInfoItemNF = class( TObjectList ) // Usado EDI 5.0  Registro 544
   private
     function  GetItem(Index: Integer): TItemNF;
     procedure SetItem(Index: Integer; Value: TItemNF);
   public
     function  New: TItemNF;
     property  Items[Index: Integer]: TItemNF read GetItem write SetItem; default;
   end;

   TRedespacho = class  // Usado EDI 5.0  Registro 545
   private
     FIdRegistro     : String ;
     FCNPJContratante: String ;
     FCNPJEmissor    : String ;
     FFilialEmissor  : String ;
     FSerie          : String ;
     FnCTe           : String ;
     FFiller         : String ;
   public
     property IdRegistro     : String  read FIdRegistro      write FIdRegistro ;
     property CNPJContratante: String  read FCNPJContratante write FCNPJContratante ;
     property CNPJEmissor    : String  read FCNPJEmissor     write FCNPJEmissor ;
     property FilialEmissor  : String  read FFilialEmissor   write FFilialEmissor ;
     property Serie          : String  read FSerie           write FSerie ;
     property nCTe           : String  read FnCTe            write FnCTe ;
     property Filler         : String  read FFiller          write FFiller ;
   end;

   TMercadorias = class
   private
     FIdRegistro       : String ;
     FCNPJEmissorNF    : String ;
     FSerieNF          : String ;
     FnNF              : String ;
     FcOcorrencia      : Integer ;
     FdtOcorrencia     : TDate ;
     FhrOcorrencia     : TTime ;
     FcObsOcorrencia   : Integer ;
     FRomaneio         : String ;  /////////////////////////////////////
     FNumeroSAP1       : String ;  //
     FNumeroSAP2       : String ;  //
     FNumeroSAP3       : String ;  //
     FFilialEmissorCT  : String ;  //    Campos Utilizados
     FSerieCT          : String ;  //
     FnCT              : String ;  //    no EDI 5.0
     FindTipoEntrega   : Integer ; //
     FcodEmissorNF     : String ;  //
     FcFilialEmissorNF : String ;  //
     FdtChegadaDestino : TDate ;   //
     FhrChegadaDestino : TTime ;   //
     FdtInicioDescarga : TDate ;   //
     FhrInicioDescarga : TTime ;   //
     FdtTerminoDescarga: TDate ;   //
     FhrTerminoDescarga: TTime ;   //
     FdtSaidaDestino   : TDate ;   //
     FhrSaidaDestino   : TTime ;   //
     FCNPJDevolucao    : String ;  //
     FSerieNFDevolucao : String ;  //
     FnNFDevolucao     : String ;  /////////////////////////////////////
     FTextoLivre       : String ;  // Edi 3.1 ou inferior
     FFiller           : String ;
     FInfoCompl        : TInfoCompl ;
     FInfoItemNF       : TInfoItemNF ;
     FRedespacho       : TRedespacho ;
   public
     constructor Create; reintroduce;
     destructor  Destroy; override;

     property IdRegistro       : String      read FIdRegistro         write FIdRegistro ;
     property CNPJEmissorNF    : String      read FCNPJEmissorNF      write FCNPJEmissorNF ;
     property SerieNF          : String      read FSerieNF            write FSerieNF;
     property nNF              : String      read FnNF                write FnNF;
     property cOcorrencia      : Integer     read FcOcorrencia        write FcOcorrencia ;
     property dtOcorrencia     : TDate       read FdtOcorrencia       write FdtOcorrencia ;
     property hrOcorrencia     : TTime       read FhrOcorrencia       write FhrOcorrencia ;
     property cObsOcorrencia   : Integer     read FcObsOcorrencia     write FcObsOcorrencia ;
     property FilialEmissorCT  : String      read FFilialEmissorCT    write FFilialEmissorCT ;
     property SerieCT          : String      read FSerieCT            write FSerieCT ;
     property nCT              : String      read FnCT                write FnCT ;
     property indTipoEntrega   : Integer     read FindTipoEntrega     write FindTipoEntrega ;
     property codEmissorNF     : String      read FcodEmissorNF       write FcodEmissorNF ;
     property cFilialEmissorNF : String      read FcFilialEmissorNF   write FcFilialEmissorNF ;
     property Romaneio         : String      read FRomaneio           write FRomaneio ;
     property NumeroSAP1       : String      read FNumeroSAP1         write FNumeroSAP1 ;
     property NumeroSAP2       : String      read FNumeroSAP2         write FNumeroSAP2 ;
     property NumeroSAP3       : String      read FNumeroSAP3         write FNumeroSAP3 ;
     property dtChegadaDestino : TDate       read FdtChegadaDestino   write FdtChegadaDestino ;
     property hrChegadaDestino : TTime       read FhrChegadaDestino   write FhrChegadaDestino ;
     property dtInicioDescarga : TDate       read FdtInicioDescarga   write FdtInicioDescarga ;
     property hrInicioDescarga : TTime       read FhrInicioDescarga   write FhrInicioDescarga ;
     property dtTerminoDescarga: TDate       read FdtTerminoDescarga  write FdtTerminoDescarga ;
     property hrTerminoDescarga: TTime       read FhrTerminoDescarga  write FhrTerminoDescarga ;
     property dtSaidaDestino   : TDate       read FdtSaidaDestino     write FdtSaidaDestino ;
     property hrSaidaDestino   : TTime       read FhrSaidaDestino     write FhrSaidaDestino ;
     property CNPJDevolucao    : String      read FCNPJDevolucao      write FCNPJDevolucao ;
     property SerieNFDevolucao : String      read FSerieNFDevolucao   write FSerieNFDevolucao ;
     property nNFDevolucao     : String      read FnNFDevolucao       write FnNFDevolucao ;
     property TextoLivre       : String      read FTextoLivre         write FTextoLivre ;
     property Filler           : String      read FFiller             write FFiller ;
     property InfoCompl        : TInfoCompl  read FInfoCompl          write FInfoCompl ;
     property InfoItemNF       : TInfoItemNF read FInfoItemNF         write FInfoItemNF ;
     property Redespacho       : TRedespacho read FRedespacho         write FRedespacho ;
   end;

   TOcorEntrega = class( TObjectList )
   private
     function  GetItem(Index: Integer): TMercadorias;
     procedure SetItem(Index: Integer; Value: TMercadorias);
   public
     function  New: TMercadorias;
     property  Items[Index: Integer]: TMercadorias read GetItem write SetItem; default;
   end;

   TTransportadora = class(TTransportador)
   private
      FOcorEntrega: TOcorEntrega ;
   public
     constructor Create ;
     destructor  Destroy; override ;
     property    OcorEntrega: TOcorEntrega  read FOcorEntrega  write FOcorEntrega ;
   end;

   TTotOcorrencias = class
   private
     FIdRegistro: String ;
     FQtde      : Integer ;
     FFiller    : String ;
   public
     property IdRegistro: String   read FIdRegistro  write FIdRegistro ;
     property nQtde     : integer  read FQtde        write FQtde ;
     property Filler    : String   read FFiller      write FFiller ;
   end;

   { TDocto Identificação do Documento }
   TOcor = class
   private
     FIdRegistro    : String ;  // Identificador do Registro
     FIdDocto       : String ;
     FFiller        : String ;
     FTransportadora: TTransportadora ;
     FTotOcorrencias: TTotOcorrencias ;
   public
     property IdRegistro    : String           read FIdRegistro      write FIdRegistro ;
     property IdDocto       : String           read FIdDocto         write FIdDocto ;
     property Filler        : String           read FFiller          write FFiller ;
     property Transportadora: TTransportadora  read FTransportadora  write FTransportadora ;
     property TotOcorrencias: TTotOcorrencias  read FTotOcorrencias  write FTotOcorrencias ;
   end;

   { TIdentDocto Registros de Identificação do Documento }
   TInfoOcor = class( TObjectList )
   private
     function  GetItem(Index: Integer): TOcor;
     procedure SetItem(Index: Integer; Value: TOcor);
   public
     function  New: TOcor;
     property  Items[Index: Integer]: TOcor read GetItem write SetItem; default;
   end;

   TACBrEDIOcorrencia = class( TComponent )
   private
      FTxt      : TACBrTxtClass ;
      FVersao   : tveEdi ;
      FConteudo : TStringList ;
      FCabecalho: TCabecalhoEdi ;
      FInfoOcor : TInfoOcor ;

      procedure GerarCabecalho ;
      procedure GerarInfoDocto       ( Registro: TOcor ) ;
      procedure GerarTransportadora  ( Registro: TTransportadora ) ;
      procedure GerarOcorrencia      ( Registro: TOcorEntrega ) ;
      procedure GerarTotalOcorrencia ( Registro: TTotOcorrencias ) ;
      procedure GerarMercadorias     ( Registro: TMercadorias ) ;
      procedure GerarInfoComplementar( Registro: TInfoCompl ) ;
      procedure GerarItensNF         ( Registro: TInfoItemNF ) ;
      procedure GerarRedespacho      ( Registro: TRedespacho ) ;
   public
     constructor Create(AOwner: TComponent); Override ;
     destructor  Destroy; Override ;

     property Cabecalho: TCabecalhoEdi  read FCabecalho   write FCabecalho ;
     property InfoOcor : TInfoOcor      read FInfoOcor    write FInfoOcor ;
     property Conteudo : TStringList    read FConteudo    write FConteudo ;
     property Versao   : tveEdi         read FVersao      write FVersao ;

     procedure GravarArquivo(const xArquivo: String);
     procedure LimpaRegistros ;
   end;

   procedure Register;

implementation

uses pcnAuxiliar, ACBrUtil ;

{$IFNDEF FPC}
 {$R ACBrEDIOcorrencia.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrEDI', [TACBrEDIOcorrencia]);
end;

{ TOcorrencias }

constructor TMercadorias.Create;
begin
  InfoCompl  := TInfoCompl.Create ;
  InfoItemNF := TInfoItemNF.Create ;
  Redespacho := TRedespacho.Create ;
end;

destructor TMercadorias.Destroy;
begin
  inherited;
end;

{ TOcorEntrega }

function TOcorEntrega.New: TMercadorias;
begin
  Result := TMercadorias.Create ;
  Add(Result) ;
end;

function TOcorEntrega.GetItem(Index: Integer): TMercadorias;
begin
  Result := TMercadorias(inherited GetItem(Index)) ;
end;

procedure TOcorEntrega.SetItem(Index: Integer; Value: TMercadorias);
begin
  Put(Index, Value);
end;

{ TACBrEDIOcorrencia }

procedure TACBrEDIOcorrencia.GerarCabecalho ;
var
  i: Integer ;
begin
  case Versao of
     ve30,
    ve30a: if Cabecalho.Id = '' then
             Cabecalho.Id := 'OCO' +
                     Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                     Copy(OnlyNumber(TimeToStr(FCabecalho.Hora)),1,4)      +
                     IntToStr(Cabecalho.Sequencia) ;
     ve31: if Cabecalho.Id = '' then
             Cabecalho.Id := 'OCO31' +
                     Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                     FTxt.VLFill(Cabecalho.Sequencia, 3, 0, '0') ;
     ve50: if Cabecalho.Id = '' then
             Cabecalho.Id := 'OCO50'+
                     Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                     FTxt.VLFill(Cabecalho.Sequencia, 3, 0, '0') ;
  end;

  Conteudo.Add( Cabecalho.IdRegistro +
                FTxt.RFill(Cabecalho.Remetente   , 35)             +
                FTxt.RFill(Cabecalho.Destinatario, 35)             +
                FTxt.LFill(Cabecalho.Data        , 'ddmmyy', false)+
                FTxt.RFill(OnlyNumber(TimeToStr(Cabecalho.Hora)), 4, '0') +
                FTxt.RFill(Cabecalho.Id, 12)                       +
                FTxt.RFill(Cabecalho.Filler, iif(Versao = ve50, 155, 25)) ) ;

  for i := 0 to InfoOcor.Count - 1 do
  begin
    GerarInfoDocto( InfoOcor.Items[i] ) ;
    GerarTransportadora( InfoOcor.Items[i].Transportadora )  ;
    if Versao = ve50 then
      GerarTotalocorrencia( InfoOcor.Items[i].TotOcorrencias ) ;
  end;

end;

procedure TACBrEDIOcorrencia.GerarInfoDocto( Registro: TOcor ) ;
begin
  case Versao of
     ve30,
    ve30a: if Registro.IdDocto = '' then
             Registro.IdDocto := 'OCO31' +
                     Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                     Copy(OnlyNumber(TimeToStr(Cabecalho.Hora)),1,4)       +
                     IntToStr(Cabecalho.Sequencia) ;
     ve31: if Registro.IdDocto = '' then
             Registro.IdDocto := 'OCO31' +
                     Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                     FTxt.LFill(Cabecalho.Sequencia, 3) ;
     ve50: if Registro.IdDocto = '' then
               Registro.IdDocto := 'OCORR50'+
                     Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                     FTxt.LFill(Cabecalho.Sequencia, 3) ;
  end;
  Conteudo.Add( Registro.IdRegistro +
                FTxt.RFill(Registro.IdDocto, 14) +
                FTxt.RFill(Registro.Filler, iif( Versao = ve50, 233, 103)) ) ;
end;

procedure TACBrEDIOcorrencia.GerarTransportadora( Registro: TTransportadora ) ;
begin
  Conteudo.Add( Registro.IdRegistro + OnlyNumber( Registro.CNPJ )    +
                FTxt.RFill( Registro.Razao, iif( Versao = ve50, 50, 40)) +
                FTxt.RFill( Registro.Filler, iif( Versao = ve50, 183, 63) ) ) ;

  GerarOcorrencia( Registro.OcorEntrega )  ;
end;

procedure TACBrEDIOcorrencia.GerarMercadorias( Registro: TMercadorias );
var
  xTexto: String ;
begin
  xTexto := Registro.IdRegistro + OnlyNumber(Registro.CNPJEmissorNF)        +
                   FTxt.RFill(Registro.SerieNF,3)                           +
                   FTxt.LFill(Registro.nNF, iif( Versao = ve50, 9, 8) )     +
                   FTxt.LFill(Registro.cOcorrencia, 3)                      +
                   FTxt.LFill(Registro.dtOcorrencia, 'ddmmyyyy', false)     +
                   FTxt.LFill(Registro.hrOcorrencia, 'hhmm', false)         +
                   FTxt.LFill(Registro.cObsOcorrencia, 2)                   ;
  if Versao = ve50 then
  begin
    xTexto := xTexto +
                   FTxt.RFill(Registro.Romaneio, 20)                        +
                   FTxt.RFill(Registro.NumeroSAP1, 20)                      +
                   FTxt.RFill(Registro.NumeroSAP2, 20)                      +
                   FTxt.RFill(Registro.NumeroSAP3, 20)                      +
                   FTxt.RFill(Registro.FilialEmissorCT, 10)                 +
                   FTxt.RFill(Registro.SerieCT, 5)                          +
                   FTxt.LFill(Registro.nCT, 12)                             +
                   FTxt.LFill(Registro.indTipoEntrega, 1)                   +
                   FTxt.RFill(Registro.codEmissorNF, 5)                     +
                   FTxt.RFill(Registro.cFilialEmissorNF, 5)                 +
                   FTxt.LFill(Registro.dtChegadaDestino, 'ddmmyyyy', false) +
                   FTxt.LFill(Registro.hrChegadaDestino, 'hhmm', false)     +
                   FTxt.LFill(Registro.dtInicioDescarga, 'ddmmyyyy', false) +
                   FTxt.LFill(Registro.hrInicioDescarga, 'hhmm', false)     +
                   FTxt.LFill(Registro.dtTerminoDescarga, 'ddmmyyyy', false)+
                   FTxt.LFill(Registro.hrTerminoDescarga, 'hhmm', false)    +
                   FTxt.LFill(Registro.dtSaidaDestino, 'ddmmyyyy', false)   +
                   FTxt.LFill(Registro.hrSaidaDestino, 'hhmm', false)       +
                   FTxt.RFill(OnlyNumber(Registro.CNPJDevolucao), 14)       +
                   FTxt.RFill(Registro.SerieNFDevolucao, 3)                 +
                   FTxt.RFill(Registro.nNFDevolucao, 9)                     +
                   FTxt.RFill(Registro.Filler, 12) ;
  end
  else
  begin
    xTexto := xTexto +
                   FTxt.RFill(Registro.TextoLivre, 70)                      +
                   FTxt.RFill(Registro.Filler, 6) ;
  end;
  Conteudo.Add( xTexto ) ;
end;

procedure TACBrEDIOcorrencia.GerarItensNF( Registro: TInfoItemNF );
var
  i: Integer ;
begin
  for i := 0 to Registro.Count - 1 do
  begin
    Conteudo.Add( Registro.Items[i].IdRegistro                         +
                  FTxt.VLFill(Registro.Items[i].qVolumeNF , 8, 2, '0') +
                  FTxt.VLFill(Registro.Items[i].qVolEntreg, 8, 2, '0') +
                  FTxt.RFill(Registro.Items[i].cItem, 20)              +
                  FTxt.RFill(Registro.Items[i].xDescricao, 50)         +
                  FTxt.RFill(Registro.Items[i].Filler, 161) ) ;
  end;
end;

procedure TACBrEDIOcorrencia.GerarRedespacho( Registro: TRedespacho );
var
  xTexto: String ;
begin
  if Registro.FCNPJContratante <> '' then
  begin
    xTexto := Registro.IdRegistro + OnlyNumber(Registro.CNPJContratante) +
                iif(Versao = ve50, OnlyNumber(Registro.CNPJEmissor), '') +
                                   FTxt.LFill(Registro.FilialEmissor, 10)+
                                   FTxt.RFill(Registro.Serie, 5)         +
                                   FTxt.RFill(Registro.nCTe, 12)         +
                   FTxt.RFill(Registro.Filler, iif(Versao = ve50, 192, 76)) ;
    Conteudo.Add( xTexto ) ;
  end;
end;

procedure TACBrEDIOcorrencia.GerarOcorrencia( Registro: TOcorEntrega ) ;
var
  c: Integer ;
begin
  if Registro.Count = 0 then
  begin
    raise Exception.Create('Ocorrência de Entrega não Informada...,'+#13+
                           'Esta Informação é Obrigatória !!') ;
  end ;

  for c := 0 to Registro.Count - 1 do
  begin
    case FVersao of
       ve30,
      ve30a,
       ve31: begin
               GerarMercadorias( Registro.Items[c] ) ;
               GerarRedespacho( Registro.Items[c].Redespacho ) ;
             end;
       ve50: begin
               GerarMercadorias( Registro.Items[c] ) ;
               GerarInfoComplementar( Registro.Items[c].InfoCompl ) ;
               GerarItensNF( Registro.Items[c].InfoItemNF ) ;
               GerarRedespacho( Registro.Items[c].Redespacho ) ;
             end;
    end;
  end;
end;

procedure TACBrEDIOcorrencia.GerarTotalOcorrencia( Registro: TTotOcorrencias ) ;
begin
  if Registro.IdRegistro = '' then
  begin
    raise Exception.Create('Registro para finalizar o Documento de Ocorrências...,'+#13+
                           'não Informado esta informação é obrigatória !!') ;
  end ;

  Conteudo.Add( Registro.IdRegistro + FormatFloat('0000', Registro.nQtde) +
                FTxt.RFill(Registro.FFiller, 243) ) ;
end;

procedure TACBrEDIOcorrencia.GerarInfoComplementar( Registro: TInfoCompl ) ;
begin
  if Registro.xMotivo1 <> '' then
    Conteudo.Add( Registro.IdRegistro + FTxt.RFill(Registro.xMotivo1, 70) +
                                        FTxt.RFill(Registro.xMotivo2, 70) +
                                        FTxt.RFill(Registro.xMotivo3, 70) +
                                        FTxt.RFill(Registro.FFiller , 37) ) ;
end;

constructor TACBrEDIOcorrencia.Create(AOwner: TComponent) ;
begin
  inherited Create(AOwner);
  FTxt                := TACBrTxtClass.Create ;
  FConteudo           := TStringList.Create ;
  FCabecalho          := TCabecalhoEdi.Create ;
  FInfoOcor           := TInfoOcor.Create ;

  FVersao             := ve50 ;
end;

destructor TACBrEDIOcorrencia.Destroy;
begin
  FTxt.Free ;
  FConteudo.Free ;
  FCabecalho.Free ;
  FInfoOcor.Free ;
  inherited ;
end;

procedure TACBrEDIOcorrencia.GravarArquivo( const xArquivo: String ) ;
begin
  Conteudo.Clear ;

  GerarCabecalho ;
  Conteudo.SaveToFile( xArquivo ) ;

  Conteudo.Clear ;
end;

procedure TACBrEDIOcorrencia.LimpaRegistros;
begin
  InfoOcor.Clear ;
end;

{ TInfoEntrega }

function TInfoItemNF.GetItem(Index: Integer): TItemNF;
begin
  Result := TItemNF(inherited GetItem(Index)) ;
end;

function TInfoItemNF.New: TItemNF;
begin
  Result := TItemNF.Create;
  Add(Result);
end;

procedure TInfoItemNF.SetItem(Index: Integer; Value: TItemNF);
begin
  Put(Index, Value) ;
end;

{ TInfoOcor }

function TInfoOcor.New: TOcor;
begin
  Result := TOcor.Create;
  Result.Transportadora := TTransportadora.Create ;
  Result.TotOcorrencias := TTotOcorrencias.Create ;
  Add(Result);
end;

function TInfoOcor.GetItem(Index: Integer): TOcor;
begin
  Result := TOcor(inherited GetItem(Index)) ;
end;

procedure TInfoOcor.SetItem(Index: Integer; Value: TOcor);
begin
  Put(Index, Value) ;
end;

{ TTransportadora }

constructor TTransportadora.Create;
begin
  inherited Create ;
  OcorEntrega := TOcorEntrega.Create ;
end;

destructor TTransportadora.Destroy;
begin
  OcorEntrega.Free ;
  inherited;
end;

end.
