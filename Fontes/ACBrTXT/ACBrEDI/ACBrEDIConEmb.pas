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

unit ACBrEDIConEmb;

{$I ACBr.inc}

interface

uses Classes, ACBrEDI, pediConversao, ACBrTxtClass, Contnrs, ACBrUtil, SysUtils,
     ACBrBase ;

type

   TCabecalhoEDI = class(TPersistent)
   private
     FRemetente   : String ;  // Remetente do Arquivo (Gerador do Arquivo EDI)
     FDestinatario: String ;  // Destinatário do Arquivo (Recebedor do Arquivo EDI)
     FData        : String ;  // Data no Formato DDMMAA
     FHora        : String ;  // Hora no Formato HHMM
     FId          : String ;  // Identificação do Intercâmbio caso não seja informado
                              // será gerado o formato padrão (CONDDMMHHMMS)
     FSequencia   : Integer ; // Número de Controle Sequencial do Arquivo
     FFiller      : String ;  // Espaços em branco para uso futuro
   published
     property Remetente   : String  read FRemetente    write FRemetente ;
     property Destinatario: String  read FDestinatario write FDestinatario ;
     property Data        : String  read FData         write FData ;
     property Hora        : String  read FHora         write FHora ;
     property Id          : String  read FId           write FId ;
     property Sequencia   : Integer read FSequencia    write FSequencia ;
     property Filler      : String  read FFiller       write FFiller ;
   end;

   TInfoCompl = class(TPersistent)
   private
     FtpMeioTransp  : tmTransporte ;
     FvTotDespesa   : Currency ;
     FvTotISS       : Currency ;
     FflContratante : String ;
     FxSerieContata : String ;
     FCTeContratante: String ;
     FcColeta       : String ;
     FdocViagemEmb  : String ;
     FdocAutorizacao: String ;
     FxChaveAcesso  : String ;
     FcTipoDocto    : tpDocto ;
     FFiller        : String ;
   published
     property tpMeioTransp  : tmTransporte read FtpMeioTransp   write FtpMeioTransp;
     property vTotDespesa   : Currency     read FvTotDespesa    write FvTotDespesa ;
     property vTotISS       : Currency     read FvTotISS        write FvTotISS ;
     property flContratante : String       read FflContratante  write FflContratante ;
     property xSerieContata : String       read FxSerieContata  write FxSerieContata ;
     property CTeContratante: String       read FCTeContratante write FCTeContratante ;
     property cColeta       : String       read FcColeta        write FcColeta ;
     property docViagemEmb  : String       read FdocViagemEmb   write FdocViagemEmb ;
     property docAutorizacao: String       read FdocAutorizacao write FdocAutorizacao ;
     property xChaveAcesso  : String       read FxChaveAcesso   write FxChaveAcesso ;
     property cTipoDocto    : tpDocto      read FcTipoDocto     write FcTipoDocto ;
     property Filler        : String       read FFiller         write FFiller ;
   end;

   TNotas = class
   private
     FxSerie : String ;
     FxNumero: String ;
   public
     property xSerie : String read FxSerie  write FxSerie ;
     property xNumero: String read FxNumero write FxNumero ;
   end;

   TNotasConhecto = class(TObjectList)
   private
     function  GetItem(Index: Integer): TNotas;
     procedure SetItem(Index: Integer; Value: TNotas);
   public
     function  New: TNotas;
     property  Items[Index: Integer]: TNotas read GetItem write SetItem; default;
   end;

   TConhecimentos = class
   private
     FFilial     : String ;
     FSerie      : String ;
     FnCTe       : String ;
     FdtEmissao  : TDate ;
     FtpFrete    : tCondicaoFrete ;
     FvPeso      : Double ;
     FvTotFrete  : Currency ;
     FvBcIcms    : Currency ;
     FpIcms      : Double ;
     FvIcms      : Currency ;
     FvFretePeso : Currency ;
     FvFrete     : Currency ;
     FvSecCat    : Currency ;
     FvITR       : Currency ;
     FvDespacho  : Currency ;
     FvPedagio   : Currency ;
     FvAdeme     : Currency ;
     FST         : TediSimNao ;
     FFiller     : String ;
     FCNPJEmissor: String ;
     FCNPJEmbarq : String ;
     FAcao       : tpAcao ;
     FTipoCTe    : tpCTe ;
     FContinua   : String ;
     FCFOP       : String ;
     FNFConhecto : TNotasConhecto ;
     FRg329      : TInfoCompl ;
   public
     constructor Create; reintroduce;
     destructor  Destroy; override;

     property Rg329      : TInfoCompl     read FRg329       write FRg329 ;
     property Filial     : String         read FFilial      write FFilial ;
     property Serie      : String         read FSerie       write FSerie;
     property nCTe       : String         read FnCTe        write FnCTe;
     property dtEmissao  : TDate          read FdtEmissao   write FdtEmissao ;
     property tpFrete    : tCondicaoFrete read FtpFrete     write FtpFrete ;
     property vPeso      : Double         read FvPeso       write FvPeso ;
     property vTotFrete  : Currency       read FvTotFrete   write FvTotFrete;
     property vBcIcms    : Currency       read FvBcIcms     write FvBcIcms ;
     property pIcms      : Double         read FpIcms       write FpIcms ;
     property vIcms      : Currency       read FvIcms       write FvIcms ;
     property vFretePeso : Currency       read FvFretePeso  write FvFretePeso ;
     property vFrete     : Currency       read FvFrete      write FvFrete ;
     property vSecCat    : Currency       read FvSecCat     write FvSecCat ;
     property vITR       : Currency       read FvITR        write FvITR ;
     property vDespacho  : Currency       read FvDespacho   write FvDespacho ;
     property vPedagio   : Currency       read FvPedagio    write FvPedagio ;
     property vAdeme     : Currency       read FvAdeme      write FvAdeme ;
     property ST         : TediSimNao     read FST          write FST ;
     property Filler     : String         read FFiller      write FFiller ;
     property CNPJEmissor: String         read FCNPJEmissor write FCNPJEmissor ;
     property CNPJEmbarq : String         read FCNPJEmbarq  write FCNPJEmbarq ;
     property Acao       : tpAcao         read FAcao        write FAcao ;
     property TipoCTe    : tpCTe          read FTipoCTe     write FTipoCTe ;
     property Continua   : String         read FContinua    write FContinua ;
     property CFOP       : String         read FCFOP        write FCFOP ;
     property NFConhecto : TNotasConhecto read FNFConhecto  write FNFConhecto ;
   end;

   TConhectoEmbarcado = class(TObjectList)
   private
     function  GetItem(Index: Integer): TConhecimentos;
     procedure SetItem(Index: Integer; Value: TConhecimentos);
   public
     function  New: TConhecimentos;
     property  Items[Index: Integer]: TConhecimentos read GetItem write SetItem; default;
   end;

   TDadosTransportadora = class(TPersistent)
   private
     FCNPJ    : String ;
     FRazao   : String ;
     FFiller  : String ;
   published
     property CNPJ    : String read FCNPJ     write FCNPJ ;
     property Razao   : String read FRazao    write FRazao ;
     property Filler  : String read FFiller   write FFiller ;
   end;

   TRegistroTCE = class(TPersistent)
   private
     FQtde    : Integer ;
     FvTotal  : Currency ;
     FFiller  : String ;
   published
     property nQtde : integer  read FQtde   write FQtde ;
     property vTotal: Currency read FvTotal write FvTotal ;
     property Filler: String   read FFiller write FFiller ;
   end;

   TDoctoEDI = class
   private
     FIdDocto : String ;
     FFiller  : String ;
   public
     property IdDocto : String     read FIdDocto write FIdDocto ;
     property Filler  : String     read FFiller  write FFiller ;
   end;

   TCabecDoctoEDI = class(TObjectList)
   private
     function  GetItem(Index: Integer): TDoctoEDI;
     procedure SetItem(Index: Integer; Value: TDoctoEDI);
   public
     function    New: TDoctoEDI;
     property    Items[Index: Integer]: TDoctoEDI read GetItem write SetItem; default;
   end;

   TConEmb = class(TObject)
   private
      FTxt    : TACBrTxtClass ;
      FnSeq   : Integer ;
      FxVersao: tveEdi ;
      //FxPath  : String ;

      FRg000  : TCabecalhoEdi ;
      FRg320  : TCabecDoctoEdi ;
      FRg321  : TDadosTransportadora ;
      FRg322  : TConhectoEmbarcado ;
      FRg329  : TInfoCompl ;
      FRg323  : TRegistroTCE ;
      function GeraRg000: String ;
      function GeraRg320( xRg: TDoctoEDI ): String ;
      function GeraRg321: String ;
      function GeraRg322(xRg: TConhecimentos): String ;
      function GeraRg329(xRg: TConhecimentos): String ;
      function GeraRg323: String ;
   public
     constructor Create;
     destructor  Destroy; Override ;
     property Rg000: TCabecalhoEdi        read FRg000 write FRg000 ;
     property Rg320: TCabecDoctoEdi       read FRg320 write FRg320 ;
     property Rg321: TDadosTransportadora read FRg321 write FRg321 ;
     property Rg322: TConhectoEmbarcado   read FRg322 write FRg322 ;
     property Rg329: TInfoCompl           read FRg329 write FRg329 ;
     property Rg323: TRegistroTCE         read FRg323 write FRg323 ;

     procedure GerarArquivo;
   end;


implementation

{ TConhecimentos }

constructor TConhecimentos.Create;
begin
  FNFConhecto := TNotasConhecto.Create ;
  FRg329      := TInfoCompl.Create ;
end;

destructor TConhecimentos.Destroy;
begin
  FNFConhecto.Free ;
  FRg329.Free ;
  inherited;
end;

{ TCabecDoctoEDI }

function TCabecDoctoEDI.New: TDoctoEDI;
begin
  Result := TDoctoEDI.Create;
  Add(Result);
end;

function TCabecDoctoEDI.GetItem(Index: Integer): TDoctoEDI;
begin
  Result := TDoctoEDI(inherited GetItem(Index)) ;
end;

procedure TCabecDoctoEDI.SetItem(Index: Integer; Value: TDoctoEDI);
begin
  Put(Index, Value) ;
end;

{ TConhectoEmbarcado }

function TConhectoEmbarcado.New: TConhecimentos;
begin
  Result := TConhecimentos.Create ;
  Add(Result) ;
end;

function TConhectoEmbarcado.GetItem(Index: Integer): TConhecimentos;
begin
  Result := TConhecimentos(inherited GetItem(Index)) ;
end;

procedure TConhectoEmbarcado.SetItem(Index: Integer; Value: TConhecimentos);
begin
  Put(Index, Value);
end;

{ TNotasConhecto }

function TNotasConhecto.New: TNotas;
begin
  Result := TNotas.Create ;
  Add(Result) ;
end;

function TNotasConhecto.GetItem(Index: Integer): TNotas;
begin
  Result := TNotas(inherited GetItem(Index)) ;
end;

procedure TNotasConhecto.SetItem(Index: Integer; Value: TNotas);
begin
  Put(Index, Value);
end;

{ TConEmb }

function TConEmb.GeraRg000: string ;
var
  tmFiller: Integer ;
begin
  tmFiller := 255 ;
  case FxVersao of
    ve30 : ;
    ve30a: ;
    ve31 : begin
             tmFiller := 585 ;
             if FRg000.FId = '' then
               FRg000.FId := 'CON31'+Copy(FRg000.FData,1,4)+FTxt.VLFill(FnSeq, 3, 0, '0') ;
           end;
    ve50 : begin
             tmFiller := 255 ;
             if FRg000.FId = '' then
               FRg000.FId := 'CON50'+Copy(FRg000.FData,1,4)+FTxt.VLFill(FnSeq, 3, 0, '0') ;
           end;
  end;


  result := '000' + FTxt.RFill(FRg000.Remetente,35)    +
                    FTxt.RFill(FRg000.Destinatario,35) +
                    FTxt.RFill(FRg000.FData,6)         +
                    FTxt.RFill(FRg000.FHora,4)         +
                    FTxt.RFill(FRg000.FId,12)          +
                    FTxt.RFill(FRg000.FFiller, tmFiller) ;
end;

function TConEmb.GeraRg320( xRg: TDoctoEDI ): String;
begin
  result := '320' + FTxt.RFill(xRg.FIdDocto,14) + FTxt.RFill(xRg.FFiller, 663) ;
end;

function TConEmb.GeraRg321: String;
begin
  result := '321' + FTxt.LFill(OnlyNumber(FRg321.FCNPJ),14) +
                    FTxt.RFill(FRg321.FRazao,40)            +
                    FTxt.RFill(FRg321.FFiller, 623) ;
end;

function TConEmb.GeraRg322(xRg: TConhecimentos): String ;
var
  xTexto, xNotas: String ;
  n: Integer ;
begin
  xTexto := '322' + Copy(xRg.FFilial,1,10)    +
                    FTxt.RFill(xRg.FSerie,5)  +
                    FTxt.RFill(xRg.FnCTe, 12) +
                    FTxt.LFill(xRg.FdtEmissao) +
                    CondicaoFreteToStr(xRg.FtpFrete)         +
                    FTxt.VLFill(xRg.FvPeso, 5, 2, '0')       +
                    FTxt.VLFill(xRg.FvTotFrete, 13, 2, '0')  +
                    FTxt.VLFill(xRg.FvBcIcms, 13, 2, '0')    +
                    FTxt.VLFill(xRg.FpIcms, 2, 2, '0')       +
                    FTxt.VLFill(xRg.FvIcms, 13, 2, '0')      +
                    FTxt.VLFill(xRg.FvFretePeso, 13, 2, '0') +
                    FTxt.VLFill(xRg.FvFrete, 13, 2, '0')     +
                    FTxt.VLFill(xRg.FvSecCat, 13, 2, '0')    +
                    FTxt.VLFill(xRg.FvITR, 13, 2, '0')       +
                    FTxt.VLFill(xRg.FvDespacho, 13, 2, '0')  +
                    FTxt.VLFill(xRg.FvPedagio, 13, 2, '0')   +
                    FTxt.VLFill(xRg.FvAdeme, 13, 2, '0')     +
                    simNaoEdiToStr(xRg.FST)                  +
                    FTxt.RFill(xRg.FFiller,3)                +
                    OnlyNumber(xRg.FCNPJEmissor)             +
                    OnlyNumber(xRg.FCNPJEmbarq) ;
  xNotas := '' ;
  for n := 0 to xRg.NFConhecto.Count -1 do
  begin
    xNotas := Trim(xNotas) + FTxt.RFill(xRg.NFConhecto.Items[n].FxSerie,3) +
                             FTxt.LFill(xRg.NFConhecto.Items[n].FxNumero,8) ;
  end;
  xTexto := xTexto + FTxt.RFill(xNotas,440)     +
                     AcaoEdiToStr(xRg.FAcao)    +
                     TipoCTeToStr(xRg.FTipoCte) +
                     xRg.FContinua              +
                     FTxt.RFill(xRg.FCFOP,5) ;
  result := xTexto ;
end;

function TConEmb.GeraRg323: String;
begin
  result := '323' + FormatFloat('0000',FRg322.Count)        +
                    FTxt.VLFill(FRg323.FvTotal, 13, 2, '0') +
                    FTxt.RFill(FRg323.FFiller,658) ;
end;

function TConEmb.GeraRg329(xRg: TConhecimentos): String;
begin
  result := '329' + MeioTransporteToStr(xRg.FRg329.FtpMeioTransp) +
                    FTxt.VLFill(xRg.FRg329.FvTotDespesa, 13, 2, '0') +
                    FTxt.VLFill(xRg.FRg329.FvTotISS, 13, 2, '0')     +
                    Copy(xRg.FRg329.FflContratante,1,10)             +
                    FTxt.RFill(xRg.FRg329.FxSerieContata,5)          +
                    FTxt.RFill(xRg.FRg329.FCTeContratante, 12)       +
                    FTxt.RFill(xRg.FRg329.FcColeta,15)               +
                    FTxt.RFill(xRg.FRg329.FdocViagemEmb,20)          +
                    FTxt.RFill(xRg.FRg329.FdocAutorizacao,20)        +
                    FTxt.RFill(xRg.FRg329.FxChaveAcesso,44)          +
                    TpDoctoToStr(xRg.FRg329.FcTipoDocto)             +
                    FTxt.RFill(xRg.FRg329.FFiller,513) ;
end;

constructor TConEmb.Create;
begin
  inherited;
  FRg000 := TCabecalhoEdi.Create ;
  FRg320 := TCabecDoctoEdi.Create ;
  FRg321 := TDadosTransportadora.Create ;
  FRg322 := TConhectoEmbarcado.Create ;
  FRg323 := TRegistroTCE.Create ;
end;

destructor TConEmb.Destroy;
begin
  FTxt.Free ;
  FRg000.Free ;
  FRg320.Free ;
  FRg321.Free ;
  FRg322.Free ;
  FRg323.Free ;
  inherited Destroy;
end;

procedure TConEmb.GerarArquivo;
var
  i, c: Integer ;
begin
  FTxt.Conteudo.Add(GeraRg000) ;
  for i := 0 to FRg320.Count - 1 do
  begin
    FTxt.Conteudo.Add(GeraRg320(FRg320.Items[i])) ;
    FTxt.Conteudo.Add(GeraRg321) ;
    for c := 0 to FRg322.Count - 1 do
    begin
      FTxt.Conteudo.Add(GeraRg322(FRg322.Items[c])) ;
      FTxt.Conteudo.Add(GeraRg329(FRg322.Items[c])) ;
    end;
    FTxt.Conteudo.Add(GeraRg323) ;
  end;
end;

end.
