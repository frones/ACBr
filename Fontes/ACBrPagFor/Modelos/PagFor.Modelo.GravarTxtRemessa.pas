{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit PagFor.Modelo.GravarTxtRemessa;

interface

uses
  SysUtils, Classes,
  ACBrPagForClass,
  CNAB240.GravarTxtRemessa;

type
 { TArquivoW_Modelo }

  TArquivoW_Modelo = class(TArquivoW_CNAB240)
  protected
    {
    procedure GeraRegistro0; override;

    procedure GeraRegistro1(I: Integer); override;

    procedure GeraRegistro5(I: Integer); override;

    procedure GeraRegistro9; override;

    procedure GeraSegmentoA(I: Integer); override;

    procedure GeraSegmentoB(mSegmentoBList: TSegmentoBList); override;

    procedure GeraSegmentoC(mSegmentoCList: TSegmentoCList); override;

    procedure GeraSegmentoJ(I: Integer); override;

    procedure GeraSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List); override;

    procedure GeraSegmentoJ53(mSegmentoJ53List: TSegmentoJ53List); override;

    procedure GeraSegmentoN1(I: Integer); override;

    procedure GeraSegmentoN2(I: Integer); override;

    procedure GeraSegmentoN3(I: Integer); override;

    procedure GeraSegmentoN4(I: Integer); override;

    procedure GeraSegmentoN567(I: Integer); override;

    procedure GeraSegmentoN8(I: Integer); override;

    procedure GeraSegmentoN9(I: Integer); override;

    procedure GeraSegmentoO(I: Integer); override;

    procedure GeraSegmentoW(mSegmentoWList: TSegmentoWList); override;

    procedure GeraSegmentoZ(mSegmentoZList: TSegmentoZList); override;
    }
  end;

implementation

end.
