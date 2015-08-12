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

unit ACBrECFBloco_L;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  /// Registro L001 - Abertura do Bloco L – Lucro Real
  TRegistroL001 = class(TOpenBlocos)
  end;

  /// Registro L030 - Identificação dos Períodos e Formas de Apuração do
  /// IRPJ e da CSLL no Ano-Calendário
  TRegistroL030 = class(TBlocos)
  private
    fDT_FIN:   TDateTime;
    fPER_APUR: string;
    fDT_INI:   TDateTime;
  public
    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property PER_APUR: string read fPER_APUR write fPER_APUR;
  end;

  /// Registro L100 - Balanço Patrimonial

  { TRegistroL100 }

  TRegistroL100 = class(TBlocos)
  private
    fIND_VAL_CTA_REF_FIN: string;
    fIND_VAL_CTA_REF_INI: string;
    fVALOR_SALDO_INICIAL: variant;
    fDESCRICAO: string;
    fCOD_CTA_SUP: string;
    fCOD_NAT: string;
    fNIVEL:  integer;
    fCODIGO: string;
    fTIPO:   string;
    fVAL_CTA_REF_FIN: variant;
    fVAL_CTA_REF_INI: variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property TIPO: string read fTIPO write fTIPO;
    property NIVEL: integer read fNIVEL write fNIVEL;
    property COD_NAT: string read fCOD_NAT write fCOD_NAT;
    property COD_CTA_SUP: string read fCOD_CTA_SUP write fCOD_CTA_SUP;
    property VAL_CTA_REF_INI: variant read fVAL_CTA_REF_INI write fVAL_CTA_REF_INI;
    property IND_VAL_CTA_REF_INI: string read fIND_VAL_CTA_REF_INI write fIND_VAL_CTA_REF_INI;
    property VAL_CTA_REF_FIN: variant read fVAL_CTA_REF_FIN write fVAL_CTA_REF_FIN;
    property IND_VAL_CTA_REF_FIN: string read fIND_VAL_CTA_REF_FIN write fIND_VAL_CTA_REF_FIN;
  end;

  /// Registro L200 - Método de Avaliação do Estoque Final
  TRegistroL200 = class(TBlocos)
  private
    fIND_AVAL_ESTOQ: string;
  public
    property IND_AVAL_ESTOQ: string read fIND_AVAL_ESTOQ write fIND_AVAL_ESTOQ;
  end;

  /// Registro L210 - Informativo da Composição de Custos
  TRegistroL210 = class(TBlocos)
  private
    fCODIGO:    string;
    fVALOR:     variant;
    fDESCRICAO: string;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro L300 - Demonstração do Resultado do Exercício
  TRegistroL300 = class(TBlocos)
  private
    fIND_VALOR: string;
    fDESCRICAO: string;
    fCOD_CTA_SUP: string;
    fVALOR:   variant;
    fCOD_NAT: string;
    fNIVEL:   integer;
    fCODIGO:  string;
    fTIPO:    string;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property TIPO: string read fTIPO write fTIPO;
    property NIVEL: integer read fNIVEL write fNIVEL;
    property COD_NAT: string read fCOD_NAT write fCOD_NAT;
    property COD_CTA_SUP: string read fCOD_CTA_SUP write fCOD_CTA_SUP;
    property VALOR: variant read fVALOR write fVALOR;
    property IND_VALOR: string read fIND_VALOR write fIND_VALOR;
  end;

  /// Registro L990 - ENCERRAMENTO DO BLOCO L
  TRegistroL990 = class(TCloseBlocos)
  end;

implementation

end.
