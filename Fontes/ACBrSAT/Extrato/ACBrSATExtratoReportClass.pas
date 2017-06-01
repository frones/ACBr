{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014 Daniel Simoes de Almeida               }
{                                                                              }
{ This file uses: DelphiZXIngQRCode Copyright 2008 ZXing authors,              }
{   port to Delphi, by Debenu Pty Ltd                                          }
{   URL: http://www.debenu.com/open-sourc1e/delphizxingqrcode                  }
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

{******************************************************************************
|* Historico
|*
|* 04/04/2013:  André Ferreira de Moraes
|*   Inicio do desenvolvimento
******************************************************************************}
{$I ACBr.inc}

unit ACBrSATExtratoReportClass;

interface

uses 
  Classes, SysUtils,
  ACBrSATExtratoClass,
  pcnConversao;

const
  CACBrSATExtratoReportClass_Versao = '0.2.0' ;

type

  { TACBrSATExtratoMargem }

  TACBrSATExtratoMargem = class( TPersistent )
  private
    fDireita: Integer;
    fEsquerda: Integer;
    fFundo: Integer;
    fTopo: Integer;
  public
    constructor create;
  published
    property Topo     : Integer read fTopo     write fTopo     default 2;
    property Esquerda : Integer read fEsquerda write fEsquerda default 2;
    property Fundo    : Integer read fFundo    write fFundo    default 4;
    property Direita  : Integer read fDireita  write fDireita  default 2;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}

  { TACBrSATExtratoReportClass }

  TACBrSATExtratoReportClass = class( TACBrSATExtratoClass )
  private
    fLarguraBobina: Integer;
    fMargens: TACBrSATExtratoMargem;
    fEspacoFinal: Integer;
    fLogoWidth: Integer;
    fLogoHeigth: Integer;
    fLogoStreatch: Boolean;
    fLogoAutoSize: Boolean;
    fLogoCenter: Boolean;
    fLogoVisible: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property LarguraBobina : Integer read fLarguraBobina  write fLarguraBobina default 302;
    property Margens       : TACBrSATExtratoMargem read fMargens write fMargens;
    property EspacoFinal   : Integer read fEspacoFinal write fEspacoFinal default 0;
    property LogoWidth     : Integer read fLogoWidth write fLogoWidth default 77;
    property LogoHeigth    : Integer read fLogoHeigth write fLogoHeigth default 50;
    property LogoStretch   : Boolean read fLogoStreatch write fLogoStreatch default False;
    property LogoAutoSize  : Boolean read fLogoAutoSize write fLogoAutoSize default True;
    property LogoCenter    : Boolean read fLogoCenter write fLogoCenter default True;
    property LogoVisible   : Boolean read fLogoVisible write fLogoVisible default True;
    property PrinterName;
  end ;

implementation

{ TACBrSATExtratoMargem }

constructor TACBrSATExtratoMargem.create;
begin
  inherited create;

  fDireita  := 2;
  fEsquerda := 2;
  fTopo     := 2;
  fFundo    := 4;
end;

{ TACBrSATExtratoReportClass }

constructor TACBrSATExtratoReportClass.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  fMargens := TACBrSATExtratoMargem.create;
  fLarguraBobina := 302;
  fEspacoFinal   := 0;
  fLogoWidth     := 77;
  fLogoHeigth    := 50;
  fLogoStreatch  := False;
  fLogoAutoSize  := True;
  fLogoCenter    := True;
  fLogoVisible   := True;
  fpAbout        := 'ACBrSATExtratoReportClass ver: ' + CACBrSATExtratoReportClass_Versao  ;
end;

destructor TACBrSATExtratoReportClass.Destroy;
begin
  fMargens.Free;

  inherited Destroy ;
end;

end.
