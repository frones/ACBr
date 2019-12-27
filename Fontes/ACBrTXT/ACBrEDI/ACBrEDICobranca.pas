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

unit ACBrEDICobranca;

{$I ACBr.inc}

interface

uses SysUtils, Classes, ACBrBase,
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
    ACBrEDIClass, pediConversao, ACBrTxtClass, Contnrs ;

type

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}

   TImpostos = class // Usado no EDI 5.0
   private
     FIdRegistro : String ;
     FvBCIcms    : Currency ;
     FpAliqIcms  : Currency ;
     FvIcms      : Currency ;
     FST         : tSimNaoST ;
     FvBCIcmsST  : Currency ;
     FpAliqIcmsST: Currency ;
     FvIcmsST    : Currency ;
     FvBcISS     : Currency ;
     FpAliqISS   : Currency ;
     FvISS       : Currency ;
     FvIR        : Currency ;
     FFiller     : String ;
   public
     property IdRegistro : String     read FIdRegistro     write FIdRegistro ;
     property vBCIcms    : Currency   read FvBCIcms        write FvBCIcms ;
     property pAliqIcms  : Currency   read FpAliqIcms      write FpAliqIcms ;
     property vIcms      : Currency   read FvIcms          write FvIcms ;
     property ST         : tSimNaoST  read FST             write FST ;
     property vBCIcmsST  : Currency   read FvBCIcmsST      write FvBCIcmsST ;
     property pAliqIcmsST: Currency   read FpAliqIcmsST    write FpAliqIcmsST ;
     property vIcmsST    : Currency   read FvIcmsST        write FvIcmsST ;
     property vBcISS     : Currency   read FvBcISS         write FvBcISS ;
     property pAliqISS   : Currency   read FpAliqISS       write FpAliqISS ;
     property vISS       : Currency   read FvISS           write FvISS ;
     property vIR        : Currency   read FvIR            write FvIR ;
     property Filler     : String     read FFiller         write FFiller ;
   end;

   TNotasFiscais = class( TObjectList )
   private
     function  GetItem(Index: Integer): TNFCT;
     procedure SetItem(Index: Integer; Value: TNFCT);
   public
     function  New: TNFCT;
     property  Items[Index: Integer]: TNFCT read GetItem write SetItem; default;
   end;

   TConhecimentos = class
   private
     FIdRegistro      : String ;
     FEmissorCTe      : String ;
     FSerieCTe        : String ;
     FnCTe            : String ;
     FdtEmissao       : TDate ;
     FvFrete          : Currency ;
     FCNPJEmissorCTe  : String ;
     FCNPJRemetente   : String ;
     FCNPJDestinatario: String ;
     FUFEmbarcador    : String ;      //////////////////////////////////////
     FUFEmissorCTe    : String ;      //
     FUFDestinatario  : String ;      //   Usado pelo
     FContaRazao      : String ;      //
     FcIVA            : String ;      //   EDI 5.0
     FRomaneio        : String ;      //
     FNumeroSAP1      : String ;      //
     FNumeroSAP2      : String ;      //
     FNumeroSAP3      : String ;      //
     FCTDevolucao     : tediSimNao ;  /////////////////////////////////////
     FFiller          : String ;
   public
     property IdRegistro      : String     read FIdRegistro        write FIdRegistro ;
     property EmissorCTe      : String     read FEmissorCTe        write FEmissorCTe ;
     property CNPJEmissorCTe  : String     read FCNPJEmissorCTe    write FCNPJEmissorCTe ;
     property CNPJRemetente   : String     read FCNPJRemetente     write FCNPJRemetente ;
     property CNPJDestinatario: String     read FCNPJDestinatario  write FCNPJDestinatario ;
     property SerieCTe        : String     read FSerieCTe          write FSerieCTe ;
     property nCTe            : String     read FnCTe              write FnCTe;
     property dtEmissao       : TDate      read FdtEmissao         write FdtEmissao ;
     property vFrete          : Currency   read FvFrete            write FvFrete ;
     property Filler          : String     read FFiller            write FFiller ;
     property Romaneio        : String     read FRomaneio          write FRomaneio ;
     property NumeroSAP1      : String     read FNumeroSAP1        write FNumeroSAP1 ;
     property NumeroSAP2      : String     read FNumeroSAP2        write FNumeroSAP2 ;
     property NumeroSAP3      : String     read FNumeroSAP3        write FNumeroSAP3 ;
     property UFEmbarcador    : String     read FUFEmbarcador      write FUFEmbarcador ;
     property UFEmissorCTe    : String     read FUFEmissorCTe      write FUFEmissorCTe ;
     property UFDestinatario  : String     read FUFDestinatario    write FUFDestinatario ;
     property ContaRazao      : String     read FContaRazao        write FContaRazao ;
     property cIVA            : String     read FcIVA              write FcIVA ;
     property CTDevolucao     : tediSimNao read FCTDevolucao       write FCTDevolucao ;
   end;

   TConhectos = class( TObjectList )
   private
     function  GetItem(Index: Integer): TConhecimentos;
     procedure SetItem(Index: Integer; Value: TConhecimentos);
   public
     function  New: TConhecimentos;
     property  Items[Index: Integer]: TConhecimentos read GetItem write SetItem; default;
   end;

   TTotCobranca = class
   private
     FIdRegistro: String ;
     FQtde      : Integer ;
     FvTotal    : Currency ;
     FFiller    : String ;
   public
     property IdRegistro: String    read FIdRegistro  write FIdRegistro ;
     property nQtde     : integer   read FQtde        write FQtde ;
     property vTotal    : Currency  read FvTotal      write FvTotal ;
     property Filler    : String    read FFiller      write FFiller ;
   end;

   TCobranca = class
   private
     FIdRegistro   : String ;
     FEmissorDocto : String ;
     FTipoDocto    : tpDocCobranca ;
     FSerieDocto   : String ;
     FNumeroDocto  : String ;
     FdtFatura     : TDateTime ;
     FdtVencimento : TDateTime ;
     FvDocto       : Currency ;
     FvICMS        : Currency ;
     FTipoCobranca : tpCobranca ;
     FpMulta       : Double ;
     FvJurosDiario : Currency ;
     FdtLimetePag  : TDateTime ;
     FvDescto      : Currency ;
     FcBanco       : Integer ;
     FxBanco       : String ;
     FnAgencia     : Integer ;
     FdvAgencia    : String ;
     FnConta       : Integer ;
     FdvConta      : String ;
     FAcao         : tpAcao ;
     FidPreFatura  : Integer ;
     FidComplFatura: String ;
     FCFOP         : String ;
     FcNFe         : Integer ;
     FchNFe        : String ;
     FxProtocoloNFe: String ;
     FFiller       : String ;
     FImpostos     : TImpostos ;       // Registro 553 EDI 5.0
     FConhectos    : TConhectos ;      // Registro 555 EDI 5.0 ou 353 EDI 3.0 a 3.1
     FNotasFiscais : TNotasFiscais ;   // Registro 556 EDI 5.0 ou 354 EDI 3.0 a 3.1
   public
     property IdRegistro   : String         read FIdRegistro     write FIdRegistro ;
     property EmissorDocto : String         read FEmissorDocto   write FEmissorDocto ;
     property TipoDocto    : tpDocCobranca  read FTipoDocto      write FTipoDocto ;
     property SerieDocto   : String         read FSerieDocto     write FSerieDocto ;
     property NumeroDocto  : String         read FNumeroDocto    write FNumeroDocto ;
     property dtFatura     : TDateTime      read FdtFatura       write FdtFatura ;
     property dtVencimento : TDateTime      read FdtVencimento   write FdtVencimento ;
     property vDocto       : Currency       read FvDocto         write FvDocto ;
     property vICMS        : Currency       read FvICMS          write FvICMS ;
     property TipoCobranca : tpCobranca     read FTipoCobranca   write FTipoCobranca ;
     property pMulta       : Double         read FpMulta         write FpMulta ;
     property vJurosDiario : Currency       read FvJurosDiario   write FvJurosDiario ;
     property dtLimetePag  : TDateTime      read FdtLimetePag    write FdtLimetePag ;
     property vDescto      : Currency       read FvDescto        write FvDescto ;
     property cBanco       : Integer        read FcBanco         write FcBanco ;
     property xBanco       : String         read FxBanco         write FxBanco ;
     property nAgencia     : Integer        read FnAgencia       write FnAgencia ;
     property dvAgencia    : String         read FdvAgencia      write FdvAgencia ;
     property nConta       : Integer        read FnConta         write FnConta ;
     property dvConta      : String         read FdvConta        write FdvConta ;
     property Acao         : tpAcao         read FAcao           write FAcao ;
     property idPreFatura  : Integer        read FidPreFatura    write FidPreFatura ;
     property idComplFatura: String         read FidComplFatura  write FidComplFatura ;
     property CFOP         : String         read FCFOP           write FCFOP ;
     property cNFe         : Integer        read FcNFe           write FcNFe ;
     property chNFe        : String         read FchNFe          write FchNFe ;
     property xProtocoloNFe: String         read FxProtocoloNFe  write FxProtocoloNFe ;
     property Filler       : String         read FFiller         write FFiller ;
     property NotasFiscais : TNotasFiscais  read FNotasFiscais   write FNotasFiscais ;
     property Conhectos    : TConhectos     read FConhectos      write FConhectos ;
     property Impostos     : TImpostos      read FImpostos       write FImpostos ;
   end;

   TDoctoCobranca = class( TObjectList )
   private
     function  GetItem(Index: Integer): TCobranca;
     procedure SetItem(Index: Integer; Value: TCobranca);
   public
     function    New: TCobranca;
     property    Items[Index: Integer]: TCobranca read GetItem write SetItem; default;
   end;

   TTransportadora = class(TTransportador)
   private
      FDoctoCobranca: TDoctoCobranca ;      // Registro 552 EDI 5.0 ou 352 EDI 3.0 a 3.1
   public
     constructor Create;
     destructor  Destroy; override ;

     property DoctoCobranca: TDoctoCobranca  read FDoctoCobranca  write FDoctoCobranca ;
   end;

   { TDocto Identificação do Documento }
   TDocCob = class
   private
     FIdRegistro    : String ;  // Identificador do Registro
     FIdDocto       : String ;
     FFiller        : String ;
     FTransportadora: TTransportadora ;  // Registro 551 EDI 5.0 ou 351 EDI 3.0 a 3.1
     FTotCobranca   : TTotCobranca ;     // Registro 559 EDI 5.0 ou 355 EDI 3.0 a 3.1
   public
     property IdRegistro    : String           read FIdRegistro      write FIdRegistro ;
     property IdDocto       : String           read FIdDocto         write FIdDocto ;
     property Filler        : String           read FFiller          write FFiller ;
     property Transportadora: TTransportadora  read FTransportadora  write FTransportadora ;
     property TotCobranca   : TTotCobranca     read FTotCobranca     write FTotCobranca ;
   end;

   { TIdentDocto Registros de Identificação do Documento }
   TInfoDocCob = class(TObjectList)
   private
     function  GetItem(Index: Integer): TDocCob;
     procedure SetItem(Index: Integer; Value: TDocCob);
   public
     function  New: TDocCob;
     property  Items[Index: Integer]: TDocCob read GetItem write SetItem; default;
   end;

   TACBrEDICobranca = class( TComponent )
   private
      FTxt       : TACBrTxtClass ;
      FVersao    : tveEdi ;
      FConteudo  : TStringList ;

      FCabecalho : TCabecalhoEdi ;       // Registro 000 todos EDI
      FInfoDocCob: TInfoDocCob ;         // Registro 550 EDI 5.0 ou 350 EDI 3.0 a 3.1

      procedure GerarCabecalho ;
      procedure GerarCabecalhoDocto( Registro: TDocCob ) ;
      procedure GerarTransportadora( Registro: TTransportadora ) ;
      procedure GerarDoctoCobranca ( Registro: TDoctoCobranca ) ;
      procedure GerarConhectos     ( Registro: TConhectos ) ;
      procedure GerarTotCobranca   ( Registro: TTotCobranca ) ;
      procedure GerarDocCob        ( Registro: TCobranca ) ;
      procedure GerarImpostos      ( Registro: TImpostos ) ;
      procedure GerarNotasFiscais  ( Registro: TNotasFiscais ) ;
   public
     constructor Create(AOwner: TComponent); Override ;
     destructor  Destroy; Override ;

     property Cabecalho : TCabecalhoEdi  read FCabecalho   write FCabecalho ;
     property InfoDocCob: TInfoDocCob    read FInfoDocCob  write FInfoDocCob ;
     property Conteudo  : TStringList    read FConteudo    write FConteudo ;
     property Versao    : tveEdi         read FVersao      write FVersao ;

     procedure GravarArquivo(const xArquivo: String);
     procedure LimpaRegistros;
   end;

   procedure Register;

implementation

uses pcnAuxiliar, ACBrUtil ;

procedure Register;
begin
  RegisterComponents('ACBrEDI', [TACBrEDICobranca]);
end;

{ TDoctoCobranca }

function TDoctoCobranca.New: TCobranca;
begin
  Result := TCobranca.Create;
  Result.Impostos     := TImpostos.Create ;
  Result.Conhectos    := TConhectos.Create ;
  Result.NotasFiscais := TNotasFiscais.Create ;
  Add(Result);
end;

function TDoctoCobranca.GetItem(Index: Integer): TCobranca;
begin
  Result := TCobranca(inherited GetItem(Index)) ;
end;

procedure TDoctoCobranca.SetItem(Index: Integer; Value: TCobranca);
begin
  Put(Index, Value) ;
end;

{ TConhectoEmbarcado }

function TConhectos.New: TConhecimentos;
begin
  Result := TConhecimentos.Create ;
  Add(Result) ;
end;

function TConhectos.GetItem(Index: Integer): TConhecimentos;
begin
  Result := TConhecimentos(inherited GetItem(Index)) ;
end;

procedure TConhectos.SetItem(Index: Integer; Value: TConhecimentos);
begin
  Put(Index, Value);
end;

{ TNotasConhecto }

function TNotasFiscais.New: TNFCT;
begin
  Result := TNFCT.Create ;
  Add(Result) ;
end;

function TNotasFiscais.GetItem(Index: Integer): TNFCT;
begin
  Result := TNFCT(inherited GetItem(Index)) ;
end;

procedure TNotasFiscais.SetItem(Index: Integer; Value: TNFCT);
begin
  Put(Index, Value);
end;

{ TConEmb }

procedure TACBrEDICobranca.GerarCabecalho ;
var
  i: Integer ;
begin
  case FVersao of
     ve30,
    ve30a,
     ve31: if Cabecalho.Id = '' then
             Cabecalho.Id := 'COB' +
                     Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                     Copy(OnlyNumber(TimeToStr(Cabecalho.Hora)),1,4)       +
                     IntToStr(Cabecalho.Sequencia) ;
     ve50: if Cabecalho.Id = '' then
               Cabecalho.Id := 'COB50'+
                     Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                     FTxt.VLFill(Cabecalho.Sequencia, 3, 0, '0') ;
  end;

  Conteudo.Add( Cabecalho.IdRegistro +
                FTxt.RFill(Cabecalho.Remetente   , 35)             +
                FTxt.RFill(Cabecalho.Destinatario, 35)             +
                FTxt.LFill(Cabecalho.Data        , 'ddmmyy', false)+
                FTxt.RFill(OnlyNumber(TimeToStr(Cabecalho.Hora)),4)+
                FTxt.RFill(Cabecalho.Id,12)                        +
                FTxt.RFill(Cabecalho.Filler, iif( Versao = ve50, 185, 75)) ) ;

  for i := 0 to InfoDocCob.Count - 1 do
  begin
    GerarCabecalhoDocto( InfoDocCob.Items[i] ) ;
    GerarTransportadora( InfoDocCob.Items[i].Transportadora ) ;
    GerarTotCobranca( InfoDocCob.Items[i].TotCobranca ) ;
  end;

end;

procedure TACBrEDICobranca.GerarCabecalhoDocto( Registro: TDocCob ) ;
begin
  case Versao of
     ve30,
    ve30a,
     ve31: if Registro.IdDocto = '' then
             Registro.IdDocto := 'COBRA' +
                     Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyyyy', false),1,4) +
                     Copy(OnlyNumber(TimeToStr(Cabecalho.Hora)),1,4)         +
                     IntToStr(Cabecalho.Sequencia) ;
     ve50: if Registro.IdDocto = '' then
             Registro.IdDocto := 'COBRA50'+
                     Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyyyy', false),1,4) +
                     FTxt.VLFill(Cabecalho.Sequencia, 3, 0, '0') ;
  end;
  Conteudo.Add( Registro.IdRegistro + FTxt.RFill(Registro.IdDocto,14) +
                FTxt.RFill(Registro.Filler, iif( Versao = ve50, 263, 153)) ) ;
end;

procedure TACBrEDICobranca.GerarConhectos( Registro: TConhectos );
var
  xTexto: String ;
  i: Integer ;
begin
  for i := 0 to Registro.Count - 1 do
  begin
    xTexto := Registro.Items[i].IdRegistro +
              FTxt.RFill(Registro.Items[i].EmissorCTe, 10)                   +
              FTxt.RFill(Registro.Items[i].SerieCTe, 5)                      +
              FTxt.LFill(Registro.Items[i].FnCTe, 12)                        +
              FTxt.VLFill(Registro.Items[i].vFrete  , 15, 2, '0')            +
              FTxt.LFill(Registro.Items[i].dtEmissao, 'ddmmyyyy', false)     +
              FTxt.LFill(OnlyNumber(Registro.Items[i].FCNPJRemetente)   , 14)+
              FTxt.LFill(OnlyNumber(Registro.Items[i].FCNPJDestinatario), 14)+
              FTxt.LFill(OnlyNumber(Registro.Items[i].FCNPJEmissorCTe)  , 14);
   if Versao = ve50 then
     xTexto := xTexto +
              FTxt.RFill(Registro.Items[i].UFEmbarcador, 2)              +
              FTxt.RFill(Registro.Items[i].UFEmissorCTe, 2)              +
              FTxt.RFill(Registro.Items[i].UFDestinatario, 2)            +
              FTxt.RFill(Registro.Items[i].ContaRazao, 10)               +
              FTxt.RFill(Registro.Items[i].cIVA, 2)                      +
              FTxt.RFill(Registro.Items[i].Romaneio, 20)                 +
              FTxt.RFill(Registro.Items[i].NumeroSAP1, 20)               +
              FTxt.RFill(Registro.Items[i].NumeroSAP2, 20)               +
              FTxt.RFill(Registro.Items[i].NumeroSAP3, 20)               +
              SimNaoEdiToStr(Registro.Items[i].CTDevolucao)              ;

    Conteudo.Add( xTexto + FTxt.RFill(Registro.Items[i].Filler,
                           iif( Versao = ve50, 86, 75)) ) ;
  end;
  if Registro.Count = 0 then
    raise Exception.Create('Nenhum Registro de Conhecimentos informado...,'+#13+
                           'É Obrigatório o preenchimento deste registro.' );
end;

procedure TACBrEDICobranca.GerarTransportadora( Registro: TTransportadora ) ;
begin
  Conteudo.Add( Registro.IdRegistro +
                FTxt.LFill(OnlyNumber(Registro.CNPJ), 14)                +
                FTxt.RFill(Registro.Razao, iif( Versao = ve50, 50, 40) ) +
                FTxt.RFill(Registro.Filler, iif( Versao = ve50, 213, 113)) ) ;

  GerarDoctoCobranca( Registro.DoctoCobranca ) ;
end;

procedure TACBrEDICobranca.GerarDocCob( Registro: TCobranca ) ;
var
  xTexto: String ;
begin
  xTexto := Registro.IdRegistro +
            FTxt.RFill(Registro.EmissorDocto, 10 )                 +
            FTxt.RFill(DocCobrancaToStr(Registro.TipoDocto), 1,'0')+
            FTxt.RFill(Registro.SerieDocto  ,  3)                  +
            FTxt.LFill(Registro.NumeroDocto, 10)                   +
            FTxt.LFill(Registro.dtFatura    , 'ddmmyyyy', false)   +
            FTxt.LFill(Registro.dtVencimento, 'ddmmyyyy', false)   +
            FTxt.VLFill(Registro.FvDocto    , 15, 2, '0')          +
            FTxt.RFill(TipoCobrancaToStr(Registro.TipoCobranca), 3)+

            iif(Versao = ve50, FTxt.VLFill(Registro.pMulta,  4, 2, '0'),
                               FTxt.VLFill(Registro.vICMS , 15, 2, '0'))+

            FTxt.VLFill(Registro.vJurosDiario, 15, 2, '0')        +
            FTxt.LFill(Registro.dtLimetePag , 'ddmmyyyy', false)  +
            FTxt.VLFill(Registro.vDescto    , 15, 2, '0')         +
            iif(Versao = ve50, FTxt.LFill(Registro.cBanco, 5, 0, false, '0'),'') +
            FTxt.RFill(Registro.xBanco,iif(Versao = ve50, 30, 35))+
            FTxt.LFill(Registro.nAgencia , 4, 0, false, '0')      +
            FTxt.RFill(Registro.dvAgencia, 1)                     +
            FTxt.LFill(Registro.nConta   , 10, 0, false, '0')     +
            FTxt.RFill(Registro.dvConta  , 2)                     +
            FTxt.RFill(AcaoEdiToStr(Registro.Acao),1,'I')         ;
   if Versao = ve50 then
     xTexto := xTexto +
            FTxt.LFill(Registro.idPreFatura, 10)                  +
            FTxt.RFill(Registro.idComplFatura, 20)                +
            FTxt.RFill(Registro.CFOP, 5)                          +
            FTxt.LFill(Registro.cNFe, 9)                          +
            FTxt.RFill(Registro.chNFe, 45)                        +
            FTxt.RFill(Registro.xProtocoloNFe, 15)                ;

  Conteudo.Add( xTexto + FTxt.RFill(Registro.Filler, iif( Versao = ve50, 20, 3)) ) ;
end;

procedure TACBrEDICobranca.GerarImpostos( Registro: TImpostos );
begin
  Conteudo.Add( Registro.IdRegistro +
                FTxt.VLFill(Registro.vIcms      , 15, 2, '0') +
                FTxt.VLFill(Registro.pAliqIcms  ,  5, 2, '0') +
                FTxt.VLFill(Registro.vBCIcms    , 15, 2, '0') +
                FTxt.VLFill(Registro.vISS       , 15, 2, '0') +
                FTxt.VLFill(Registro.pAliqISS   ,  5, 2, '0') +
                FTxt.VLFill(Registro.vIcmsST    , 15, 2, '0') +
                FTxt.VLFill(Registro.vBCIcmsST  , 15, 2, '0') +
                FTxt.VLFill(Registro.pAliqIcmsST,  5, 2, '0') +
                FTxt.VLFill(Registro.vBcISS     , 15, 2, '0') +
                FTxt.VLFill(Registro.vIR        , 15, 2, '0') +
                FTxt.RFill(Registro.Filler, 157) );
end;

procedure TACBrEDICobranca.GerarNotasFiscais( Registro: TNotasFiscais ) ;
var
  i: Integer ;
  xTexto: String ;
begin
  for i := 0 to Registro.Count - 1 do
  begin
    xTexto := Registro.Items[i].IdRegistro +
              FTxt.RFill(Registro.Items[i].xSerie, 3)                      +
              FTxt.LFill(Registro.Items[i].xNumero, iif(Versao = ve50,9,8))+
              FTxt.LFill(Registro.Items[i].dtEmissao, 'ddmmyyyy', false)   +
              FTxt.VLFill(Registro.Items[i].qPesoNF ,  7, 2, '0')          +
              FTxt.VLFill(Registro.Items[i].vNF     , 15, 2, '0')          +
              FTxt.LFill(OnlyNumber(Registro.Items[i].CNPJEmissor), 14) ;
    if Versao = ve50 then
      xTexto := xTexto +
              FTxt.RFill(Registro.Items[i].Romaneio  , 20)                 +
              FTxt.RFill(Registro.Items[i].NumeroSAP1, 20)                 +
              FTxt.RFill(Registro.Items[i].NumeroSAP2, 20)                 +
              FTxt.RFill(Registro.Items[i].NumeroSAP3, 20)                 +
              SimNaoEDIToStr(Registro.Items[i].Devolucao)          ;

    Conteudo.Add( xTexto + FTxt.RFill(Registro.Items[i].Filler,
                           iif( Versao = ve50, 140, 112)) ) ;
  end;
end;

procedure TACBrEDICobranca.GerarDoctoCobranca( Registro: TDoctoCobranca ) ;
var
  c: Integer ;
begin
  for c := 0 to Registro.Count - 1 do
  begin

    GerarDocCob      ( Registro.Items[c] ) ;
    if Versao = ve50 then
      GerarImpostos    ( Registro.Items[c].Impostos ) ;
    GerarConhectos   ( Registro.Items[c].Conhectos ) ;
    GerarNotasFiscais( Registro.Items[c].NotasFiscais ) ;

  end;

  if Registro.Count = 0 then
    raise Exception.Create('Nenhum Registro de Docto. de Cobrança informado...,'+#13+
                           'É Obrigatório o preenchimento deste registro.' );
end;

procedure TACBrEDICobranca.GerarTotCobranca( Registro: TTotCobranca );
begin
  Conteudo.Add( Registro.IdRegistro + FormatFloat('0000', Registro.nQtde) +
                FTxt.VLFill(Registro.vTotal, 15, 2, '0') +
                FTxt.RFill(Registro.Filler, iif( Versao = ve50, 258, 658)) ) ;
end;

constructor TACBrEDICobranca.Create(AOwner: TComponent) ;
begin
  inherited Create(AOwner) ;
  FTxt              := TACBrTxtClass.Create ;
  FConteudo         := TStringList.Create ;
  FCabecalho        := TCabecalhoEdi.Create ;
  FInfoDocCob       := TInfoDocCob.Create ;

  FVersao           := ve50 ;
end;

destructor TACBrEDICobranca.Destroy;
begin
  FTxt.Free ;
  FConteudo.Free ;
  FCabecalho.Free ;
  FInfoDocCob.Free ;

  inherited ;
end;

procedure TACBrEDICobranca.GravarArquivo(const xArquivo: String ) ;
begin
  Conteudo.Clear ;

  GerarCabecalho ;
  Conteudo.SaveToFile( xArquivo ) ;

  Conteudo.Clear ;
end;

procedure TACBrEDICobranca.LimpaRegistros;
begin
  InfoDocCob.Clear ;
end;

{ TInfoDocCob }

function TInfoDocCob.New: TDocCob;
begin
  Result                := TDocCob.Create;
  Result.Transportadora := TTransportadora.Create ;
  Result.TotCobranca    := TTotCobranca.Create ;
  Add(Result);
end;

function TInfoDocCob.GetItem(Index: Integer): TDocCob;
begin
  Result := TDocCob(inherited GetItem(Index)) ;
end;

procedure TInfoDocCob.SetItem(Index: Integer; Value: TDocCob);
begin
  Put(Index, Value) ;
end;

{ TTransportadora }

constructor TTransportadora.Create;
begin
  inherited Create ;
  FDoctoCobranca := TDoctoCobranca.Create ;
end;

destructor TTransportadora.Destroy;
begin
  FDoctoCobranca.Free ;
  inherited;
end;

end.
