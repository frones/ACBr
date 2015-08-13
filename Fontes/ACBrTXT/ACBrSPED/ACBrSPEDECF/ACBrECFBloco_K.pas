{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014   Juliomar Marchetti                   }
{					  Isaque Pinheiro		       }
{ 					  Daniel Simões de Almeida	       }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_K;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  /// Registro K001 - Abertura do Bloco K – Saldos das Contas Contábeis e Referenciais
  TRegistroK001 = class(TOpenBlocos)
  end;

  /// Registro K030 - Identificação dos Períodos e Formas de Apuração do IRPJ e da CSLL no Ano-Calendário
  TRegistroK030 = class(TBlocos)
  private
    fPER_APUR: string;
    fDT_FIN:   TDateTime;
    fDT_INI:   TDateTime;
  public
    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property PER_APUR: string read fPER_APUR write fPER_APUR;
  end;

  /// Registro K155 - Detalhes dos Saldos Contábeis (Depois do
  /// Encerramento do Resultado do Período)

  { TRegistroK155 }

  TRegistroK155 = class(TBlocos)
  private
    fCOD_CTA:    string;
    fCOD_CCUS:   string;
    fIND_VL_SLD_FIN: string;
    fVL_SLD_FIN: variant;
    fVL_SLD_INI: variant;
    fIND_VL_SLD_FIN: string;
    fIND_VL_SLD_INI: string;
    fVL_CRED:    variant;
    fVL_DEB:     variant;
  public
    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_SLD_INI: variant read fVL_SLD_INI write fVL_SLD_INI;
    property IND_VL_SLD_INI: string read fIND_VL_SLD_INI write fIND_VL_SLD_INI;
    property VL_DEB: variant read fVL_DEB write fVL_DEB;
    property VL_CRED: variant read fVL_CRED write fVL_CRED;
    property VL_SLD_FIN: variant read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;
  end;

  /// Registro K156 - Mapeamento Referencial do Saldo Final
  TRegistroK156 = class(TBlocos)
  private
    fVL_SLD_FIN:     variant;
    fIND_VL_SLD_FIN: string;
    fCOD_CTA_REF:    string;
  public
    property COD_CTA_REF: string read fCOD_CTA_REF write fCOD_CTA_REF;
    property VL_SLD_FIN: variant read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;
  end;

  /// Registro K355 - Saldos Finais das Contas Contábeis de Resultado
  /// Antes do Encerramento
  TRegistroK355 = class(TBlocos)
  private
    fCOD_CTA:    string;
    fVL_SLD_FIN: variant;
    fCOD_CCUS:   string;
    fIND_VL_SLD_FIN: string;
  public
    property COD_CTA: string read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: string read fCOD_CCUS write fCOD_CCUS;
    property VL_SLD_FIN: variant read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;
  end;

  /// Registro K356 - Mapeamento Referencial dos Saldos Finais das
  /// Contas de Resultado Antes do Encerramento
  TRegistroK356 = class(TBlocos)
  private
    fVL_SLD_FIN:     variant;
    fIND_VL_SLD_FIN: string;
    fCOD_CTA_REF:    string;
  public
    property COD_CTA_REF: string read fCOD_CTA_REF write fCOD_CTA_REF;
    property VL_SLD_FIN: variant read fVL_SLD_FIN write fVL_SLD_FIN;
    property IND_VL_SLD_FIN: string read fIND_VL_SLD_FIN write fIND_VL_SLD_FIN;
  end;

  /// Registro K990 - ENCERRAMENTO DO BLOCO K
  TRegistroK990 = class(TCloseBlocos)
  end;

implementation

end.
