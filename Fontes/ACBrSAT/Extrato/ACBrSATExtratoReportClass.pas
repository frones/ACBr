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

type

  { TACBrSATExtratoReportClass }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or
  pidiOSSimulator or  pidAndroid or
  pidLinux32 or pidiOSDevice
  {$IFDEF RTL300_UP}
  or pidiOSDevice32 or pidLinux64
  or pidWinNX32 or pidWinIoT32
  or pidiOSDevice64
  or pidOSX64 or pidLinux32Arm
  or pidLinux64Arm or pidAndroid64Arm
  {$ENDIF RTL300_UP})]
  {$ENDIF RTL230_UP}
  TACBrSATExtratoReportClass = class( TACBrSATExtratoClass )
  private
    fLarguraBobina: Integer;
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
    property EspacoFinal   : Integer read fEspacoFinal write fEspacoFinal default 0;
    property LogoWidth     : Integer read fLogoWidth write fLogoWidth default 77;
    property LogoHeigth    : Integer read fLogoHeigth write fLogoHeigth default 50;
    property LogoStretch   : Boolean read fLogoStreatch write fLogoStreatch default False;
    property LogoAutoSize  : Boolean read fLogoAutoSize write fLogoAutoSize default True;
    property LogoCenter    : Boolean read fLogoCenter write fLogoCenter default True;
    property LogoVisible   : Boolean read fLogoVisible write fLogoVisible default True;
  end ;

implementation

{ TACBrSATExtratoReportClass }
constructor TACBrSATExtratoReportClass.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  fLarguraBobina := 302;
  fEspacoFinal   := 0;
  fLogoWidth     := 77;
  fLogoHeigth    := 50;
  fLogoStreatch  := False;
  fLogoAutoSize  := True;
  fLogoCenter    := True;
  fLogoVisible   := True;
  MargemInferior := 4;
  MargemSuperior := 2;
  MargemEsquerda := 2;
  MargemDireita := 2;
end;

destructor TACBrSATExtratoReportClass.Destroy;
begin
  inherited Destroy ;
end;

end.
