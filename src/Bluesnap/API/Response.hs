{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Bluesnap.API.Response
  ( module Bluesnap.API.Response
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
data Web_authentication = Web_authentication
        { web_authentication_token :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Web_authentication where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Web_authentication
            `apply` elementToken
    schemaTypeToXML s x@Web_authentication{} =
        toXMLElement s []
            [ elementToXMLToken $ web_authentication_token x
            ]
 
elementWeb_authentication :: XMLParser Web_authentication
elementWeb_authentication = parseSchemaType "web-authentication"
elementToXMLWeb_authentication :: Web_authentication -> [Content ()]
elementToXMLWeb_authentication = schemaTypeToXML "web-authentication"
 
data Token = Token
        deriving (Eq,Show)
instance SchemaType Token where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Token
    schemaTypeToXML s x@Token{} =
        toXMLElement s []
            []
 
elementToken :: XMLParser Xsd.XsdString
elementToken = parseSchemaType "token"
elementToXMLToken :: Xsd.XsdString -> [Content ()]
elementToXMLToken = schemaTypeToXML "token"
 
data Batch_order = Batch_order
        { batch_order_shopper :: Shopper
        , batch_order_order :: Order
        }
        deriving (Eq,Show)
instance SchemaType Batch_order where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Batch_order
            `apply` elementShopper
            `apply` elementOrder
    schemaTypeToXML s x@Batch_order{} =
        toXMLElement s []
            [ elementToXMLShopper $ batch_order_shopper x
            , elementToXMLOrder $ batch_order_order x
            ]
 
elementBatch_order :: XMLParser Batch_order
elementBatch_order = parseSchemaType "batch-order"
elementToXMLBatch_order :: Batch_order -> [Content ()]
elementToXMLBatch_order = schemaTypeToXML "batch-order"
 
data Catalog_sku = Catalog_sku
        { catalog_sku_sku_id :: Maybe Xs.Long
        , catalog_sku_sku_name :: Xsd.XsdString
        , catalog_sku_contract_name :: Maybe Xsd.XsdString
        , catalog_sku_product_id :: Maybe Xs.Long
        , catalog_sku_sku_status :: Xsd.XsdString
        , catalog_sku_sku_type :: Xsd.XsdString
        , catalog_sku_pricing_settings :: Pricing_settings
        , catalog_sku_sku_image :: Xsd.XsdString
        , catalog_sku_sku_buynow_urls :: Sku_buynow_urls
        , catalog_sku_sku_quantity_policy :: Sku_quantity_policy
        , catalog_sku_collect_shipping_address :: Xsd.Boolean
        , catalog_sku_sku_effective_dates :: Sku_effective_dates
        , catalog_sku_sku_coupon_settings :: Sku_coupon_settings
        , catalog_sku_sku_custom_parameters :: Sku_custom_parameters
        }
        deriving (Eq,Show)
instance SchemaType Catalog_sku where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Catalog_sku
            `apply` optional (elementSku_id)
            `apply` elementSku_name
            `apply` optional (elementContract_name)
            `apply` optional (elementProduct_id)
            `apply` elementSku_status
            `apply` elementSku_type
            `apply` elementPricing_settings
            `apply` elementSku_image
            `apply` elementSku_buynow_urls
            `apply` elementSku_quantity_policy
            `apply` elementCollect_shipping_address
            `apply` elementSku_effective_dates
            `apply` elementSku_coupon_settings
            `apply` elementSku_custom_parameters
    schemaTypeToXML s x@Catalog_sku{} =
        toXMLElement s []
            [ maybe [] (elementToXMLSku_id) $ catalog_sku_sku_id x
            , elementToXMLSku_name $ catalog_sku_sku_name x
            , maybe [] (elementToXMLContract_name) $ catalog_sku_contract_name x
            , maybe [] (elementToXMLProduct_id) $ catalog_sku_product_id x
            , elementToXMLSku_status $ catalog_sku_sku_status x
            , elementToXMLSku_type $ catalog_sku_sku_type x
            , elementToXMLPricing_settings $ catalog_sku_pricing_settings x
            , elementToXMLSku_image $ catalog_sku_sku_image x
            , elementToXMLSku_buynow_urls $ catalog_sku_sku_buynow_urls x
            , elementToXMLSku_quantity_policy $ catalog_sku_sku_quantity_policy x
            , elementToXMLCollect_shipping_address $ catalog_sku_collect_shipping_address x
            , elementToXMLSku_effective_dates $ catalog_sku_sku_effective_dates x
            , elementToXMLSku_coupon_settings $ catalog_sku_sku_coupon_settings x
            , elementToXMLSku_custom_parameters $ catalog_sku_sku_custom_parameters x
            ]
 
elementCatalog_sku :: XMLParser Catalog_sku
elementCatalog_sku = parseSchemaType "catalog-sku"
elementToXMLCatalog_sku :: Catalog_sku -> [Content ()]
elementToXMLCatalog_sku = schemaTypeToXML "catalog-sku"
 
data Pricing_settings = Pricing_settings
        { pricing_settings_charge_policy_type :: Xsd.XsdString
        , pricing_settings_charge_policy :: Charge_policy
        , pricing_settings_include_tax_in_price :: Xsd.Boolean
        , pricing_settings_rounding_price_method :: Xsd.XsdString
        , pricing_settings_recurring_plan_settings :: Recurring_plan_settings
        }
        deriving (Eq,Show)
instance SchemaType Pricing_settings where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Pricing_settings
            `apply` elementCharge_policy_type
            `apply` elementCharge_policy
            `apply` elementInclude_tax_in_price
            `apply` elementRounding_price_method
            `apply` elementRecurring_plan_settings
    schemaTypeToXML s x@Pricing_settings{} =
        toXMLElement s []
            [ elementToXMLCharge_policy_type $ pricing_settings_charge_policy_type x
            , elementToXMLCharge_policy $ pricing_settings_charge_policy x
            , elementToXMLInclude_tax_in_price $ pricing_settings_include_tax_in_price x
            , elementToXMLRounding_price_method $ pricing_settings_rounding_price_method x
            , elementToXMLRecurring_plan_settings $ pricing_settings_recurring_plan_settings x
            ]
 
elementPricing_settings :: XMLParser Pricing_settings
elementPricing_settings = parseSchemaType "pricing-settings"
elementToXMLPricing_settings :: Pricing_settings -> [Content ()]
elementToXMLPricing_settings = schemaTypeToXML "pricing-settings"
 
data Recurring_plan_settings = Recurring_plan_settings
        { recurring_plan_settings_charge_upon_plan_change :: Xsd.Boolean
        , recurring_plan_settings_grace_period_length :: Xsd.XsdString
        , recurring_plan_settings_plan_charge_amount_limit :: Plan_charge_amount_limit
        , recurring_plan_settings_plan_duration_period :: Xsd.Decimal
        , recurring_plan_settings_plan_max_charge_number_limit :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Recurring_plan_settings where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Recurring_plan_settings
            `apply` elementCharge_upon_plan_change
            `apply` elementGrace_period_length
            `apply` elementPlan_charge_amount_limit
            `apply` elementPlan_duration_period
            `apply` elementPlan_max_charge_number_limit
    schemaTypeToXML s x@Recurring_plan_settings{} =
        toXMLElement s []
            [ elementToXMLCharge_upon_plan_change $ recurring_plan_settings_charge_upon_plan_change x
            , elementToXMLGrace_period_length $ recurring_plan_settings_grace_period_length x
            , elementToXMLPlan_charge_amount_limit $ recurring_plan_settings_plan_charge_amount_limit x
            , elementToXMLPlan_duration_period $ recurring_plan_settings_plan_duration_period x
            , elementToXMLPlan_max_charge_number_limit $ recurring_plan_settings_plan_max_charge_number_limit x
            ]
 
elementRecurring_plan_settings :: XMLParser Recurring_plan_settings
elementRecurring_plan_settings = parseSchemaType "recurring-plan-settings"
elementToXMLRecurring_plan_settings :: Recurring_plan_settings -> [Content ()]
elementToXMLRecurring_plan_settings = schemaTypeToXML "recurring-plan-settings"
 
data Charge_policy = Charge_policy
        { charge_policy_free_trial :: Free_trial
        , charge_policy_one_time_charge :: One_time_charge
        , charge_policy_initial_period :: Initial_period
        , charge_policy_recurring_period :: Recurring_period
        }
        deriving (Eq,Show)
instance SchemaType Charge_policy where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Charge_policy
            `apply` elementFree_trial
            `apply` elementOne_time_charge
            `apply` elementInitial_period
            `apply` elementRecurring_period
    schemaTypeToXML s x@Charge_policy{} =
        toXMLElement s []
            [ elementToXMLFree_trial $ charge_policy_free_trial x
            , elementToXMLOne_time_charge $ charge_policy_one_time_charge x
            , elementToXMLInitial_period $ charge_policy_initial_period x
            , elementToXMLRecurring_period $ charge_policy_recurring_period x
            ]
 
elementCharge_policy :: XMLParser Charge_policy
elementCharge_policy = parseSchemaType "charge-policy"
elementToXMLCharge_policy :: Charge_policy -> [Content ()]
elementToXMLCharge_policy = schemaTypeToXML "charge-policy"
 
data Free_trial = Free_trial
        { free_trial_period_length :: Xsd.XsdString
        , free_trial_interval :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Free_trial where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Free_trial
            `apply` elementPeriod_length
            `apply` elementInterval
    schemaTypeToXML s x@Free_trial{} =
        toXMLElement s []
            [ elementToXMLPeriod_length $ free_trial_period_length x
            , elementToXMLInterval $ free_trial_interval x
            ]
 
elementFree_trial :: XMLParser Free_trial
elementFree_trial = parseSchemaType "free-trial"
elementToXMLFree_trial :: Free_trial -> [Content ()]
elementToXMLFree_trial = schemaTypeToXML "free-trial"
 
data Sku_custom_parameters = Sku_custom_parameters
        { sku_custom_parameters_sku_custom_parameter :: [Sku_custom_parameter]
        }
        deriving (Eq,Show)
instance SchemaType Sku_custom_parameters where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_custom_parameters
            `apply` many (elementSku_custom_parameter)
    schemaTypeToXML s x@Sku_custom_parameters{} =
        toXMLElement s []
            [ concatMap (elementToXMLSku_custom_parameter) $ sku_custom_parameters_sku_custom_parameter x
            ]
 
elementSku_custom_parameters :: XMLParser Sku_custom_parameters
elementSku_custom_parameters = parseSchemaType "sku-custom-parameters"
elementToXMLSku_custom_parameters :: Sku_custom_parameters -> [Content ()]
elementToXMLSku_custom_parameters = schemaTypeToXML "sku-custom-parameters"
 
data Sku_custom_parameter = Sku_custom_parameter
        { sku_custom_parameter_url :: Maybe Xsd.XsdString
        , sku_custom_parameter_custom_param_id :: Maybe Xs.Long
        , sku_custom_parameter_index_number :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Sku_custom_parameter where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_custom_parameter
            `apply` optional (elementUrl)
            `apply` optional (elementCustom_param_id)
            `apply` elementIndex_number
    schemaTypeToXML s x@Sku_custom_parameter{} =
        toXMLElement s []
            [ maybe [] (elementToXMLUrl) $ sku_custom_parameter_url x
            , maybe [] (elementToXMLCustom_param_id) $ sku_custom_parameter_custom_param_id x
            , elementToXMLIndex_number $ sku_custom_parameter_index_number x
            ]
 
elementSku_custom_parameter :: XMLParser Sku_custom_parameter
elementSku_custom_parameter = parseSchemaType "sku-custom-parameter"
elementToXMLSku_custom_parameter :: Sku_custom_parameter -> [Content ()]
elementToXMLSku_custom_parameter = schemaTypeToXML "sku-custom-parameter"
 
data Index_number = Index_number
        deriving (Eq,Show)
instance SchemaType Index_number where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Index_number
    schemaTypeToXML s x@Index_number{} =
        toXMLElement s []
            []
 
elementIndex_number :: XMLParser Xsd.Decimal
elementIndex_number = parseSchemaType "index-number"
elementToXMLIndex_number :: Xsd.Decimal -> [Content ()]
elementToXMLIndex_number = schemaTypeToXML "index-number"
 
data One_time_charge = One_time_charge
        { one_time_charge_catalog_prices :: Catalog_prices
        }
        deriving (Eq,Show)
instance SchemaType One_time_charge where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return One_time_charge
            `apply` elementCatalog_prices
    schemaTypeToXML s x@One_time_charge{} =
        toXMLElement s []
            [ elementToXMLCatalog_prices $ one_time_charge_catalog_prices x
            ]
 
elementOne_time_charge :: XMLParser One_time_charge
elementOne_time_charge = parseSchemaType "one-time-charge"
elementToXMLOne_time_charge :: One_time_charge -> [Content ()]
elementToXMLOne_time_charge = schemaTypeToXML "one-time-charge"
 
data Initial_period = Initial_period
        { initial_period_catalog_prices :: Catalog_prices
        , initial_period_period_length :: Xsd.XsdString
        , initial_period_interval :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Initial_period where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Initial_period
            `apply` elementCatalog_prices
            `apply` elementPeriod_length
            `apply` elementInterval
    schemaTypeToXML s x@Initial_period{} =
        toXMLElement s []
            [ elementToXMLCatalog_prices $ initial_period_catalog_prices x
            , elementToXMLPeriod_length $ initial_period_period_length x
            , elementToXMLInterval $ initial_period_interval x
            ]
 
elementInitial_period :: XMLParser Initial_period
elementInitial_period = parseSchemaType "initial-period"
elementToXMLInitial_period :: Initial_period -> [Content ()]
elementToXMLInitial_period = schemaTypeToXML "initial-period"
 
data Recurring_period = Recurring_period
        { recurring_period_catalog_prices :: Catalog_prices
        , recurring_period_period_frequency :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Recurring_period where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Recurring_period
            `apply` elementCatalog_prices
            `apply` elementPeriod_frequency
    schemaTypeToXML s x@Recurring_period{} =
        toXMLElement s []
            [ elementToXMLCatalog_prices $ recurring_period_catalog_prices x
            , elementToXMLPeriod_frequency $ recurring_period_period_frequency x
            ]
 
elementRecurring_period :: XMLParser Recurring_period
elementRecurring_period = parseSchemaType "recurring-period"
elementToXMLRecurring_period :: Recurring_period -> [Content ()]
elementToXMLRecurring_period = schemaTypeToXML "recurring-period"
 
data Catalog_prices = Catalog_prices
        { catalog_prices_catalog_price :: [Catalog_price]
        }
        deriving (Eq,Show)
instance SchemaType Catalog_prices where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Catalog_prices
            `apply` many (elementCatalog_price)
    schemaTypeToXML s x@Catalog_prices{} =
        toXMLElement s []
            [ concatMap (elementToXMLCatalog_price) $ catalog_prices_catalog_price x
            ]
 
elementCatalog_prices :: XMLParser Catalog_prices
elementCatalog_prices = parseSchemaType "catalog-prices"
elementToXMLCatalog_prices :: Catalog_prices -> [Content ()]
elementToXMLCatalog_prices = schemaTypeToXML "catalog-prices"
 
data Catalog_price = Catalog_price
        { catalog_price_base_price :: Maybe Xsd.Boolean
        , catalog_price_currency :: Xsd.XsdString
        , catalog_price_amount :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Catalog_price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Catalog_price
            `apply` optional (elementBase_price)
            `apply` elementCurrency
            `apply` elementAmount
    schemaTypeToXML s x@Catalog_price{} =
        toXMLElement s []
            [ maybe [] (elementToXMLBase_price) $ catalog_price_base_price x
            , elementToXMLCurrency $ catalog_price_currency x
            , elementToXMLAmount $ catalog_price_amount x
            ]
 
elementCatalog_price :: XMLParser Catalog_price
elementCatalog_price = parseSchemaType "catalog-price"
elementToXMLCatalog_price :: Catalog_price -> [Content ()]
elementToXMLCatalog_price = schemaTypeToXML "catalog-price"
 
data Sku_effective_dates = Sku_effective_dates
        { sku_effective_dates_effective_from :: Xsd.XsdString
        , sku_effective_dates_effective_till :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Sku_effective_dates where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_effective_dates
            `apply` elementEffective_from
            `apply` elementEffective_till
    schemaTypeToXML s x@Sku_effective_dates{} =
        toXMLElement s []
            [ elementToXMLEffective_from $ sku_effective_dates_effective_from x
            , elementToXMLEffective_till $ sku_effective_dates_effective_till x
            ]
 
elementSku_effective_dates :: XMLParser Sku_effective_dates
elementSku_effective_dates = parseSchemaType "sku-effective-dates"
elementToXMLSku_effective_dates :: Sku_effective_dates -> [Content ()]
elementToXMLSku_effective_dates = schemaTypeToXML "sku-effective-dates"
 
data Sku_quantity_policy = Sku_quantity_policy
        { sku_quantity_policy_allow_quantity_change :: Xsd.Boolean
        , sku_quantity_policy_minimum_quantity :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Sku_quantity_policy where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_quantity_policy
            `apply` elementAllow_quantity_change
            `apply` elementMinimum_quantity
    schemaTypeToXML s x@Sku_quantity_policy{} =
        toXMLElement s []
            [ elementToXMLAllow_quantity_change $ sku_quantity_policy_allow_quantity_change x
            , elementToXMLMinimum_quantity $ sku_quantity_policy_minimum_quantity x
            ]
 
elementSku_quantity_policy :: XMLParser Sku_quantity_policy
elementSku_quantity_policy = parseSchemaType "sku-quantity-policy"
elementToXMLSku_quantity_policy :: Sku_quantity_policy -> [Content ()]
elementToXMLSku_quantity_policy = schemaTypeToXML "sku-quantity-policy"
 
data Sku_buynow_urls = Sku_buynow_urls
        { sku_buynow_urls_buynow_url :: [Buynow_url]
        }
        deriving (Eq,Show)
instance SchemaType Sku_buynow_urls where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_buynow_urls
            `apply` many (elementBuynow_url)
    schemaTypeToXML s x@Sku_buynow_urls{} =
        toXMLElement s []
            [ concatMap (elementToXMLBuynow_url) $ sku_buynow_urls_buynow_url x
            ]
 
elementSku_buynow_urls :: XMLParser Sku_buynow_urls
elementSku_buynow_urls = parseSchemaType "sku-buynow-urls"
elementToXMLSku_buynow_urls :: Sku_buynow_urls -> [Content ()]
elementToXMLSku_buynow_urls = schemaTypeToXML "sku-buynow-urls"
 
data Buynow_url = Buynow_url
        { buynow_url_url_description :: Xsd.XsdString
        , buynow_url_url :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Buynow_url where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Buynow_url
            `apply` elementUrl_description
            `apply` elementUrl
    schemaTypeToXML s x@Buynow_url{} =
        toXMLElement s []
            [ elementToXMLUrl_description $ buynow_url_url_description x
            , elementToXMLUrl $ buynow_url_url x
            ]
 
elementBuynow_url :: XMLParser Buynow_url
elementBuynow_url = parseSchemaType "buynow-url"
elementToXMLBuynow_url :: Buynow_url -> [Content ()]
elementToXMLBuynow_url = schemaTypeToXML "buynow-url"
 
data Sku_coupon_settings = Sku_coupon_settings
        { sku_coupon_settings_sku_coupon_setting :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Sku_coupon_settings where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_coupon_settings
            `apply` elementSku_coupon_setting
    schemaTypeToXML s x@Sku_coupon_settings{} =
        toXMLElement s []
            [ elementToXMLSku_coupon_setting $ sku_coupon_settings_sku_coupon_setting x
            ]
 
elementSku_coupon_settings :: XMLParser Sku_coupon_settings
elementSku_coupon_settings = parseSchemaType "sku-coupon-settings"
elementToXMLSku_coupon_settings :: Sku_coupon_settings -> [Content ()]
elementToXMLSku_coupon_settings = schemaTypeToXML "sku-coupon-settings"
 
data Url_description = Url_description
        deriving (Eq,Show)
instance SchemaType Url_description where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Url_description
    schemaTypeToXML s x@Url_description{} =
        toXMLElement s []
            []
 
elementUrl_description :: XMLParser Xsd.XsdString
elementUrl_description = parseSchemaType "url-description"
elementToXMLUrl_description :: Xsd.XsdString -> [Content ()]
elementToXMLUrl_description = schemaTypeToXML "url-description"
 
data Sku_image = Sku_image
        deriving (Eq,Show)
instance SchemaType Sku_image where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_image
    schemaTypeToXML s x@Sku_image{} =
        toXMLElement s []
            []
 
elementSku_image :: XMLParser Xsd.XsdString
elementSku_image = parseSchemaType "sku-image"
elementToXMLSku_image :: Xsd.XsdString -> [Content ()]
elementToXMLSku_image = schemaTypeToXML "sku-image"
 
data Sku_type = Sku_type
        deriving (Eq,Show)
instance SchemaType Sku_type where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_type
    schemaTypeToXML s x@Sku_type{} =
        toXMLElement s []
            []
 
elementSku_type :: XMLParser Xsd.XsdString
elementSku_type = parseSchemaType "sku-type"
elementToXMLSku_type :: Xsd.XsdString -> [Content ()]
elementToXMLSku_type = schemaTypeToXML "sku-type"
 
data Collect_shipping_address = Collect_shipping_address
        deriving (Eq,Show)
instance SchemaType Collect_shipping_address where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Collect_shipping_address
    schemaTypeToXML s x@Collect_shipping_address{} =
        toXMLElement s []
            []
 
elementCollect_shipping_address :: XMLParser Xsd.Boolean
elementCollect_shipping_address = parseSchemaType "collect-shipping-address"
elementToXMLCollect_shipping_address :: Xsd.Boolean -> [Content ()]
elementToXMLCollect_shipping_address = schemaTypeToXML "collect-shipping-address"
 
data Effective_from = Effective_from
        deriving (Eq,Show)
instance SchemaType Effective_from where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Effective_from
    schemaTypeToXML s x@Effective_from{} =
        toXMLElement s []
            []
 
elementEffective_from :: XMLParser Xsd.XsdString
elementEffective_from = parseSchemaType "effective-from"
elementToXMLEffective_from :: Xsd.XsdString -> [Content ()]
elementToXMLEffective_from = schemaTypeToXML "effective-from"
 
data Effective_till = Effective_till
        deriving (Eq,Show)
instance SchemaType Effective_till where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Effective_till
    schemaTypeToXML s x@Effective_till{} =
        toXMLElement s []
            []
 
elementEffective_till :: XMLParser Xsd.XsdString
elementEffective_till = parseSchemaType "effective-till"
elementToXMLEffective_till :: Xsd.XsdString -> [Content ()]
elementToXMLEffective_till = schemaTypeToXML "effective-till"
 
data Allow_quantity_change = Allow_quantity_change
        deriving (Eq,Show)
instance SchemaType Allow_quantity_change where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Allow_quantity_change
    schemaTypeToXML s x@Allow_quantity_change{} =
        toXMLElement s []
            []
 
elementAllow_quantity_change :: XMLParser Xsd.Boolean
elementAllow_quantity_change = parseSchemaType "allow-quantity-change"
elementToXMLAllow_quantity_change :: Xsd.Boolean -> [Content ()]
elementToXMLAllow_quantity_change = schemaTypeToXML "allow-quantity-change"
 
data Base_price = Base_price
        deriving (Eq,Show)
instance SchemaType Base_price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Base_price
    schemaTypeToXML s x@Base_price{} =
        toXMLElement s []
            []
 
elementBase_price :: XMLParser Xsd.Boolean
elementBase_price = parseSchemaType "base-price"
elementToXMLBase_price :: Xsd.Boolean -> [Content ()]
elementToXMLBase_price = schemaTypeToXML "base-price"
 
data Minimum_quantity = Minimum_quantity
        deriving (Eq,Show)
instance SchemaType Minimum_quantity where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Minimum_quantity
    schemaTypeToXML s x@Minimum_quantity{} =
        toXMLElement s []
            []
 
elementMinimum_quantity :: XMLParser Xsd.XsdString
elementMinimum_quantity = parseSchemaType "minimum-quantity"
elementToXMLMinimum_quantity :: Xsd.XsdString -> [Content ()]
elementToXMLMinimum_quantity = schemaTypeToXML "minimum-quantity"
 
data Buynow1_url = Buynow1_url
        deriving (Eq,Show)
instance SchemaType Buynow1_url where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Buynow1_url
    schemaTypeToXML s x@Buynow1_url{} =
        toXMLElement s []
            []
 
elementBuynow1_url :: XMLParser Xsd.XsdString
elementBuynow1_url = parseSchemaType "buynow1-url"
elementToXMLBuynow1_url :: Xsd.XsdString -> [Content ()]
elementToXMLBuynow1_url = schemaTypeToXML "buynow1-url"
 
data Sku_initial_price = Sku_initial_price
        { sku_initial_price_currency :: Xsd.XsdString
        , sku_initial_price_amount :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Sku_initial_price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_initial_price
            `apply` elementCurrency
            `apply` elementAmount
    schemaTypeToXML s x@Sku_initial_price{} =
        toXMLElement s []
            [ elementToXMLCurrency $ sku_initial_price_currency x
            , elementToXMLAmount $ sku_initial_price_amount x
            ]
 
elementSku_initial_price :: XMLParser Sku_initial_price
elementSku_initial_price = parseSchemaType "sku-initial-price"
elementToXMLSku_initial_price :: Sku_initial_price -> [Content ()]
elementToXMLSku_initial_price = schemaTypeToXML "sku-initial-price"
 
data Sku_recurring_price = Sku_recurring_price
        { sku_recurring_price_currency :: Xsd.XsdString
        , sku_recurring_price_amount :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Sku_recurring_price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_recurring_price
            `apply` elementCurrency
            `apply` elementAmount
    schemaTypeToXML s x@Sku_recurring_price{} =
        toXMLElement s []
            [ elementToXMLCurrency $ sku_recurring_price_currency x
            , elementToXMLAmount $ sku_recurring_price_amount x
            ]
 
elementSku_recurring_price :: XMLParser Sku_recurring_price
elementSku_recurring_price = parseSchemaType "sku-recurring-price"
elementToXMLSku_recurring_price :: Sku_recurring_price -> [Content ()]
elementToXMLSku_recurring_price = schemaTypeToXML "sku-recurring-price"
 
data Charge_policy_type = Charge_policy_type
        deriving (Eq,Show)
instance SchemaType Charge_policy_type where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Charge_policy_type
    schemaTypeToXML s x@Charge_policy_type{} =
        toXMLElement s []
            []
 
elementCharge_policy_type :: XMLParser Xsd.XsdString
elementCharge_policy_type = parseSchemaType "charge-policy-type"
elementToXMLCharge_policy_type :: Xsd.XsdString -> [Content ()]
elementToXMLCharge_policy_type = schemaTypeToXML "charge-policy-type"
 
data Subscription_plan_frequency = Subscription_plan_frequency
        deriving (Eq,Show)
instance SchemaType Subscription_plan_frequency where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Subscription_plan_frequency
    schemaTypeToXML s x@Subscription_plan_frequency{} =
        toXMLElement s []
            []
 
elementSubscription_plan_frequency :: XMLParser Xsd.XsdString
elementSubscription_plan_frequency = parseSchemaType "subscription-plan-frequency"
elementToXMLSubscription_plan_frequency :: Xsd.XsdString -> [Content ()]
elementToXMLSubscription_plan_frequency = schemaTypeToXML "subscription-plan-frequency"
 
data Subscription_trial_period = Subscription_trial_period
        deriving (Eq,Show)
instance SchemaType Subscription_trial_period where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Subscription_trial_period
    schemaTypeToXML s x@Subscription_trial_period{} =
        toXMLElement s []
            []
 
elementSubscription_trial_period :: XMLParser Xsd.XsdString
elementSubscription_trial_period = parseSchemaType "subscription-trial-period"
elementToXMLSubscription_trial_period :: Xsd.XsdString -> [Content ()]
elementToXMLSubscription_trial_period = schemaTypeToXML "subscription-trial-period"
 
data Plan_duration_period = Plan_duration_period
        deriving (Eq,Show)
instance SchemaType Plan_duration_period where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Plan_duration_period
    schemaTypeToXML s x@Plan_duration_period{} =
        toXMLElement s []
            []
 
elementPlan_duration_period :: XMLParser Xsd.Decimal
elementPlan_duration_period = parseSchemaType "plan-duration-period"
elementToXMLPlan_duration_period :: Xsd.Decimal -> [Content ()]
elementToXMLPlan_duration_period = schemaTypeToXML "plan-duration-period"
 
data Charge_upon_plan_change = Charge_upon_plan_change
        deriving (Eq,Show)
instance SchemaType Charge_upon_plan_change where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Charge_upon_plan_change
    schemaTypeToXML s x@Charge_upon_plan_change{} =
        toXMLElement s []
            []
 
elementCharge_upon_plan_change :: XMLParser Xsd.Boolean
elementCharge_upon_plan_change = parseSchemaType "charge-upon-plan-change"
elementToXMLCharge_upon_plan_change :: Xsd.Boolean -> [Content ()]
elementToXMLCharge_upon_plan_change = schemaTypeToXML "charge-upon-plan-change"
 
data Grace_period_length = Grace_period_length
        deriving (Eq,Show)
instance SchemaType Grace_period_length where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Grace_period_length
    schemaTypeToXML s x@Grace_period_length{} =
        toXMLElement s []
            []
 
elementGrace_period_length :: XMLParser Xsd.XsdString
elementGrace_period_length = parseSchemaType "grace-period-length"
elementToXMLGrace_period_length :: Xsd.XsdString -> [Content ()]
elementToXMLGrace_period_length = schemaTypeToXML "grace-period-length"
 
data Plan_charge_amount_limit = Plan_charge_amount_limit
        { plan_charge_amount_limit_currency :: Xsd.XsdString
        , plan_charge_amount_limit_amount :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Plan_charge_amount_limit where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Plan_charge_amount_limit
            `apply` elementCurrency
            `apply` elementAmount
    schemaTypeToXML s x@Plan_charge_amount_limit{} =
        toXMLElement s []
            [ elementToXMLCurrency $ plan_charge_amount_limit_currency x
            , elementToXMLAmount $ plan_charge_amount_limit_amount x
            ]
 
elementPlan_charge_amount_limit :: XMLParser Plan_charge_amount_limit
elementPlan_charge_amount_limit = parseSchemaType "plan-charge-amount-limit"
elementToXMLPlan_charge_amount_limit :: Plan_charge_amount_limit -> [Content ()]
elementToXMLPlan_charge_amount_limit = schemaTypeToXML "plan-charge-amount-limit"
 
data Include_tax_in_price = Include_tax_in_price
        deriving (Eq,Show)
instance SchemaType Include_tax_in_price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Include_tax_in_price
    schemaTypeToXML s x@Include_tax_in_price{} =
        toXMLElement s []
            []
 
elementInclude_tax_in_price :: XMLParser Xsd.Boolean
elementInclude_tax_in_price = parseSchemaType "include-tax-in-price"
elementToXMLInclude_tax_in_price :: Xsd.Boolean -> [Content ()]
elementToXMLInclude_tax_in_price = schemaTypeToXML "include-tax-in-price"
 
data Rounding_price_method = Rounding_price_method
        deriving (Eq,Show)
instance SchemaType Rounding_price_method where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Rounding_price_method
    schemaTypeToXML s x@Rounding_price_method{} =
        toXMLElement s []
            []
 
elementRounding_price_method :: XMLParser Xsd.XsdString
elementRounding_price_method = parseSchemaType "rounding-price-method"
elementToXMLRounding_price_method :: Xsd.XsdString -> [Content ()]
elementToXMLRounding_price_method = schemaTypeToXML "rounding-price-method"
 
data Sku_coupon_setting = Sku_coupon_setting
        deriving (Eq,Show)
instance SchemaType Sku_coupon_setting where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_coupon_setting
    schemaTypeToXML s x@Sku_coupon_setting{} =
        toXMLElement s []
            []
 
elementSku_coupon_setting :: XMLParser Xsd.XsdString
elementSku_coupon_setting = parseSchemaType "sku-coupon-setting"
elementToXMLSku_coupon_setting :: Xsd.XsdString -> [Content ()]
elementToXMLSku_coupon_setting = schemaTypeToXML "sku-coupon-setting"
 
data Plan_max_charge_number_limit = Plan_max_charge_number_limit
        deriving (Eq,Show)
instance SchemaType Plan_max_charge_number_limit where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Plan_max_charge_number_limit
    schemaTypeToXML s x@Plan_max_charge_number_limit{} =
        toXMLElement s []
            []
 
elementPlan_max_charge_number_limit :: XMLParser Xsd.XsdString
elementPlan_max_charge_number_limit = parseSchemaType "plan-max-charge-number-limit"
elementToXMLPlan_max_charge_number_limit :: Xsd.XsdString -> [Content ()]
elementToXMLPlan_max_charge_number_limit = schemaTypeToXML "plan-max-charge-number-limit"
 
data Period_length = Period_length
        deriving (Eq,Show)
instance SchemaType Period_length where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Period_length
    schemaTypeToXML s x@Period_length{} =
        toXMLElement s []
            []
 
elementPeriod_length :: XMLParser Xsd.XsdString
elementPeriod_length = parseSchemaType "period-length"
elementToXMLPeriod_length :: Xsd.XsdString -> [Content ()]
elementToXMLPeriod_length = schemaTypeToXML "period-length"
 
data Interval = Interval
        deriving (Eq,Show)
instance SchemaType Interval where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Interval
    schemaTypeToXML s x@Interval{} =
        toXMLElement s []
            []
 
elementInterval :: XMLParser Xsd.XsdString
elementInterval = parseSchemaType "interval"
elementToXMLInterval :: Xsd.XsdString -> [Content ()]
elementToXMLInterval = schemaTypeToXML "interval"
 
data Period_frequency = Period_frequency
        deriving (Eq,Show)
instance SchemaType Period_frequency where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Period_frequency
    schemaTypeToXML s x@Period_frequency{} =
        toXMLElement s []
            []
 
elementPeriod_frequency :: XMLParser Xsd.XsdString
elementPeriod_frequency = parseSchemaType "period-frequency"
elementToXMLPeriod_frequency :: Xsd.XsdString -> [Content ()]
elementToXMLPeriod_frequency = schemaTypeToXML "period-frequency"
 
data Number_of_charges = Number_of_charges
        deriving (Eq,Show)
instance SchemaType Number_of_charges where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Number_of_charges
    schemaTypeToXML s x@Number_of_charges{} =
        toXMLElement s []
            []
 
elementNumber_of_charges :: XMLParser Xsd.XsdString
elementNumber_of_charges = parseSchemaType "number-of-charges"
elementToXMLNumber_of_charges :: Xsd.XsdString -> [Content ()]
elementToXMLNumber_of_charges = schemaTypeToXML "number-of-charges"
 
data Zip = Zip
        deriving (Eq,Show)
instance SchemaType Zip where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Zip
    schemaTypeToXML s x@Zip{} =
        toXMLElement s []
            []
 
elementZip :: XMLParser Xsd.XsdString
elementZip = parseSchemaType "zip"
elementToXMLZip :: Xsd.XsdString -> [Content ()]
elementToXMLZip = schemaTypeToXML "zip"
 
data State = State
        deriving (Eq,Show)
instance SchemaType State where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return State
    schemaTypeToXML s x@State{} =
        toXMLElement s []
            []
 
elementState :: XMLParser Xsd.XsdString
elementState = parseSchemaType "state"
elementToXMLState :: Xsd.XsdString -> [Content ()]
elementToXMLState = schemaTypeToXML "state"
 
data Last_name = Last_name
        deriving (Eq,Show)
instance SchemaType Last_name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Last_name
    schemaTypeToXML s x@Last_name{} =
        toXMLElement s []
            []
 
elementLast_name :: XMLParser Xsd.XsdString
elementLast_name = parseSchemaType "last-name"
elementToXMLLast_name :: Xsd.XsdString -> [Content ()]
elementToXMLLast_name = schemaTypeToXML "last-name"
 
data First_name = First_name
        deriving (Eq,Show)
instance SchemaType First_name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return First_name
    schemaTypeToXML s x@First_name{} =
        toXMLElement s []
            []
 
elementFirst_name :: XMLParser Xsd.XsdString
elementFirst_name = parseSchemaType "first-name"
elementToXMLFirst_name :: Xsd.XsdString -> [Content ()]
elementToXMLFirst_name = schemaTypeToXML "first-name"
 
data Country = Country
        deriving (Eq,Show)
instance SchemaType Country where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Country
    schemaTypeToXML s x@Country{} =
        toXMLElement s []
            []
 
elementCountry :: XMLParser Xsd.XsdString
elementCountry = parseSchemaType "country"
elementToXMLCountry :: Xsd.XsdString -> [Content ()]
elementToXMLCountry = schemaTypeToXML "country"
 
data City = City
        deriving (Eq,Show)
instance SchemaType City where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return City
    schemaTypeToXML s x@City{} =
        toXMLElement s []
            []
 
elementCity :: XMLParser Xsd.XsdString
elementCity = parseSchemaType "city"
elementToXMLCity :: Xsd.XsdString -> [Content ()]
elementToXMLCity = schemaTypeToXML "city"
 
data Address2 = Address2
        deriving (Eq,Show)
instance SchemaType Address2 where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Address2
    schemaTypeToXML s x@Address2{} =
        toXMLElement s []
            []
 
elementAddress2 :: XMLParser Xsd.XsdString
elementAddress2 = parseSchemaType "address2"
elementToXMLAddress2 :: Xsd.XsdString -> [Content ()]
elementToXMLAddress2 = schemaTypeToXML "address2"
 
data Address1 = Address1
        deriving (Eq,Show)
instance SchemaType Address1 where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Address1
    schemaTypeToXML s x@Address1{} =
        toXMLElement s []
            []
 
elementAddress1 :: XMLParser Xsd.XsdString
elementAddress1 = parseSchemaType "address1"
elementToXMLAddress1 :: Xsd.XsdString -> [Content ()]
elementToXMLAddress1 = schemaTypeToXML "address1"
 
data Batch_coupon = Batch_coupon
        { batch_coupon_cart_info :: Cart_info
        , batch_coupon_coupons_info :: Maybe Coupons_info
        }
        deriving (Eq,Show)
instance SchemaType Batch_coupon where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Batch_coupon
            `apply` elementCart_info
            `apply` optional (elementCoupons_info)
    schemaTypeToXML s x@Batch_coupon{} =
        toXMLElement s []
            [ elementToXMLCart_info $ batch_coupon_cart_info x
            , maybe [] (elementToXMLCoupons_info) $ batch_coupon_coupons_info x
            ]
 
elementBatch_coupon :: XMLParser Batch_coupon
elementBatch_coupon = parseSchemaType "batch-coupon"
elementToXMLBatch_coupon :: Batch_coupon -> [Content ()]
elementToXMLBatch_coupon = schemaTypeToXML "batch-coupon"
 
data Coupons_info = Coupons_info
        { coupons_info_coupon_info :: [Coupon_info]
        }
        deriving (Eq,Show)
instance SchemaType Coupons_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupons_info
            `apply` many (elementCoupon_info)
    schemaTypeToXML s x@Coupons_info{} =
        toXMLElement s []
            [ concatMap (elementToXMLCoupon_info) $ coupons_info_coupon_info x
            ]
 
elementCoupons_info :: XMLParser Coupons_info
elementCoupons_info = parseSchemaType "coupons-info"
elementToXMLCoupons_info :: Coupons_info -> [Content ()]
elementToXMLCoupons_info = schemaTypeToXML "coupons-info"
 
data Coupon_info = Coupon_info
        { coupon_info_coupon_id :: Maybe Xs.Long
        , coupon_info_product_owner_id :: Maybe Xs.Long
        , coupon_info_coupon_name :: Xsd.XsdString
        , coupon_info_coupon_display_name :: Xsd.XsdString
        , coupon_info_minimal_amount :: Xsd.Decimal
        , coupon_info_usage_count_limit :: Xs.Int
        , coupon_info_coupon_effective_date :: Xsd.XsdString
        , coupon_info_coupon_expiration_date :: Maybe Xsd.XsdString
        , coupon_info_one_per_email :: Xsd.Boolean
        , coupon_info_apply_recurring_charges :: Xsd.Boolean
        , coupon_info_applicable_cycles :: Maybe Xs.Int
        , coupon_info_coupon_discount_policy :: Coupon_discount_policy
        , coupon_info_coupon_rules :: Coupon_rules
        , coupon_info_coupon_codes_info :: Coupon_codes_info
        }
        deriving (Eq,Show)
instance SchemaType Coupon_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_info
            `apply` optional (elementCoupon_id)
            `apply` optional (elementProduct_owner_id)
            `apply` elementCoupon_name
            `apply` elementCoupon_display_name
            `apply` elementMinimal_amount
            `apply` elementUsage_count_limit
            `apply` elementCoupon_effective_date
            `apply` optional (elementCoupon_expiration_date)
            `apply` elementOne_per_email
            `apply` elementApply_recurring_charges
            `apply` optional (elementApplicable_cycles)
            `apply` elementCoupon_discount_policy
            `apply` elementCoupon_rules
            `apply` elementCoupon_codes_info
    schemaTypeToXML s x@Coupon_info{} =
        toXMLElement s []
            [ maybe [] (elementToXMLCoupon_id) $ coupon_info_coupon_id x
            , maybe [] (elementToXMLProduct_owner_id) $ coupon_info_product_owner_id x
            , elementToXMLCoupon_name $ coupon_info_coupon_name x
            , elementToXMLCoupon_display_name $ coupon_info_coupon_display_name x
            , elementToXMLMinimal_amount $ coupon_info_minimal_amount x
            , elementToXMLUsage_count_limit $ coupon_info_usage_count_limit x
            , elementToXMLCoupon_effective_date $ coupon_info_coupon_effective_date x
            , maybe [] (elementToXMLCoupon_expiration_date) $ coupon_info_coupon_expiration_date x
            , elementToXMLOne_per_email $ coupon_info_one_per_email x
            , elementToXMLApply_recurring_charges $ coupon_info_apply_recurring_charges x
            , maybe [] (elementToXMLApplicable_cycles) $ coupon_info_applicable_cycles x
            , elementToXMLCoupon_discount_policy $ coupon_info_coupon_discount_policy x
            , elementToXMLCoupon_rules $ coupon_info_coupon_rules x
            , elementToXMLCoupon_codes_info $ coupon_info_coupon_codes_info x
            ]
 
elementCoupon_info :: XMLParser Coupon_info
elementCoupon_info = parseSchemaType "coupon-info"
elementToXMLCoupon_info :: Coupon_info -> [Content ()]
elementToXMLCoupon_info = schemaTypeToXML "coupon-info"
 
data Coupon_rules = Coupon_rules
        { coupon_rules_applied_skus_rule :: Xsd.XsdString
        , coupon_rules_coupon_exceptions :: Maybe Coupon_exceptions
        }
        deriving (Eq,Show)
instance SchemaType Coupon_rules where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_rules
            `apply` elementApplied_skus_rule
            `apply` optional (elementCoupon_exceptions)
    schemaTypeToXML s x@Coupon_rules{} =
        toXMLElement s []
            [ elementToXMLApplied_skus_rule $ coupon_rules_applied_skus_rule x
            , maybe [] (elementToXMLCoupon_exceptions) $ coupon_rules_coupon_exceptions x
            ]
 
elementCoupon_rules :: XMLParser Coupon_rules
elementCoupon_rules = parseSchemaType "coupon-rules"
elementToXMLCoupon_rules :: Coupon_rules -> [Content ()]
elementToXMLCoupon_rules = schemaTypeToXML "coupon-rules"
 
data Coupon_codes_info = Coupon_codes_info
        { coupon_codes_info_coupon_code_info :: [Coupon_code_info]
        }
        deriving (Eq,Show)
instance SchemaType Coupon_codes_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_codes_info
            `apply` many (elementCoupon_code_info)
    schemaTypeToXML s x@Coupon_codes_info{} =
        toXMLElement s []
            [ concatMap (elementToXMLCoupon_code_info) $ coupon_codes_info_coupon_code_info x
            ]
 
elementCoupon_codes_info :: XMLParser Coupon_codes_info
elementCoupon_codes_info = parseSchemaType "coupon-codes-info"
elementToXMLCoupon_codes_info :: Coupon_codes_info -> [Content ()]
elementToXMLCoupon_codes_info = schemaTypeToXML "coupon-codes-info"
 
data Coupon_code_info = Coupon_code_info
        { coupon_code_info_coupon_code :: Maybe Xsd.XsdString
        , coupon_code_info_coupon_code_usage :: Maybe Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType Coupon_code_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_code_info
            `apply` optional (elementCoupon_code)
            `apply` optional (elementCoupon_code_usage)
    schemaTypeToXML s x@Coupon_code_info{} =
        toXMLElement s []
            [ maybe [] (elementToXMLCoupon_code) $ coupon_code_info_coupon_code x
            , maybe [] (elementToXMLCoupon_code_usage) $ coupon_code_info_coupon_code_usage x
            ]
 
elementCoupon_code_info :: XMLParser Coupon_code_info
elementCoupon_code_info = parseSchemaType "coupon-code-info"
elementToXMLCoupon_code_info :: Coupon_code_info -> [Content ()]
elementToXMLCoupon_code_info = schemaTypeToXML "coupon-code-info"
 
data Coupon_code = Coupon_code
        deriving (Eq,Show)
instance SchemaType Coupon_code where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_code
    schemaTypeToXML s x@Coupon_code{} =
        toXMLElement s []
            []
 
elementCoupon_code :: XMLParser Xsd.XsdString
elementCoupon_code = parseSchemaType "coupon-code"
elementToXMLCoupon_code :: Xsd.XsdString -> [Content ()]
elementToXMLCoupon_code = schemaTypeToXML "coupon-code"
 
data Coupon_code_usage = Coupon_code_usage
        deriving (Eq,Show)
instance SchemaType Coupon_code_usage where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_code_usage
    schemaTypeToXML s x@Coupon_code_usage{} =
        toXMLElement s []
            []
 
elementCoupon_code_usage :: XMLParser Xs.Int
elementCoupon_code_usage = parseSchemaType "coupon-code-usage"
elementToXMLCoupon_code_usage :: Xs.Int -> [Content ()]
elementToXMLCoupon_code_usage = schemaTypeToXML "coupon-code-usage"
 
data Coupon_exceptions = Coupon_exceptions
        { coupon_exceptions_sku_id :: [Xs.Long]
        }
        deriving (Eq,Show)
instance SchemaType Coupon_exceptions where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_exceptions
            `apply` many (elementSku_id)
    schemaTypeToXML s x@Coupon_exceptions{} =
        toXMLElement s []
            [ concatMap (elementToXMLSku_id) $ coupon_exceptions_sku_id x
            ]
 
elementCoupon_exceptions :: XMLParser Coupon_exceptions
elementCoupon_exceptions = parseSchemaType "coupon-exceptions"
elementToXMLCoupon_exceptions :: Coupon_exceptions -> [Content ()]
elementToXMLCoupon_exceptions = schemaTypeToXML "coupon-exceptions"
 
data Minimal_amount = Minimal_amount
        deriving (Eq,Show)
instance SchemaType Minimal_amount where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Minimal_amount
    schemaTypeToXML s x@Minimal_amount{} =
        toXMLElement s []
            []
 
elementMinimal_amount :: XMLParser Xsd.Decimal
elementMinimal_amount = parseSchemaType "minimal-amount"
elementToXMLMinimal_amount :: Xsd.Decimal -> [Content ()]
elementToXMLMinimal_amount = schemaTypeToXML "minimal-amount"
 
data Usage_count_limit = Usage_count_limit
        deriving (Eq,Show)
instance SchemaType Usage_count_limit where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Usage_count_limit
    schemaTypeToXML s x@Usage_count_limit{} =
        toXMLElement s []
            []
 
elementUsage_count_limit :: XMLParser Xs.Int
elementUsage_count_limit = parseSchemaType "usage-count-limit"
elementToXMLUsage_count_limit :: Xs.Int -> [Content ()]
elementToXMLUsage_count_limit = schemaTypeToXML "usage-count-limit"
 
data Coupon_effective_date = Coupon_effective_date
        deriving (Eq,Show)
instance SchemaType Coupon_effective_date where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_effective_date
    schemaTypeToXML s x@Coupon_effective_date{} =
        toXMLElement s []
            []
 
elementCoupon_effective_date :: XMLParser Xsd.XsdString
elementCoupon_effective_date = parseSchemaType "coupon-effective-date"
elementToXMLCoupon_effective_date :: Xsd.XsdString -> [Content ()]
elementToXMLCoupon_effective_date = schemaTypeToXML "coupon-effective-date"
 
data Coupon_expiration_date = Coupon_expiration_date
        deriving (Eq,Show)
instance SchemaType Coupon_expiration_date where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_expiration_date
    schemaTypeToXML s x@Coupon_expiration_date{} =
        toXMLElement s []
            []
 
elementCoupon_expiration_date :: XMLParser Xsd.XsdString
elementCoupon_expiration_date = parseSchemaType "coupon-expiration-date"
elementToXMLCoupon_expiration_date :: Xsd.XsdString -> [Content ()]
elementToXMLCoupon_expiration_date = schemaTypeToXML "coupon-expiration-date"
 
data One_per_email = One_per_email
        deriving (Eq,Show)
instance SchemaType One_per_email where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return One_per_email
    schemaTypeToXML s x@One_per_email{} =
        toXMLElement s []
            []
 
elementOne_per_email :: XMLParser Xsd.Boolean
elementOne_per_email = parseSchemaType "one-per-email"
elementToXMLOne_per_email :: Xsd.Boolean -> [Content ()]
elementToXMLOne_per_email = schemaTypeToXML "one-per-email"
 
data Apply_recurring_charges = Apply_recurring_charges
        deriving (Eq,Show)
instance SchemaType Apply_recurring_charges where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Apply_recurring_charges
    schemaTypeToXML s x@Apply_recurring_charges{} =
        toXMLElement s []
            []
 
elementApply_recurring_charges :: XMLParser Xsd.Boolean
elementApply_recurring_charges = parseSchemaType "apply-recurring-charges"
elementToXMLApply_recurring_charges :: Xsd.Boolean -> [Content ()]
elementToXMLApply_recurring_charges = schemaTypeToXML "apply-recurring-charges"
 
data Applicable_cycles = Applicable_cycles
        deriving (Eq,Show)
instance SchemaType Applicable_cycles where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Applicable_cycles
    schemaTypeToXML s x@Applicable_cycles{} =
        toXMLElement s []
            []
 
elementApplicable_cycles :: XMLParser Xs.Int
elementApplicable_cycles = parseSchemaType "applicable-cycles"
elementToXMLApplicable_cycles :: Xs.Int -> [Content ()]
elementToXMLApplicable_cycles = schemaTypeToXML "applicable-cycles"
 
data Applied_skus_rule = Applied_skus_rule
        deriving (Eq,Show)
instance SchemaType Applied_skus_rule where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Applied_skus_rule
    schemaTypeToXML s x@Applied_skus_rule{} =
        toXMLElement s []
            []
 
elementApplied_skus_rule :: XMLParser Xsd.XsdString
elementApplied_skus_rule = parseSchemaType "applied-skus-rule"
elementToXMLApplied_skus_rule :: Xsd.XsdString -> [Content ()]
elementToXMLApplied_skus_rule = schemaTypeToXML "applied-skus-rule"
 
data Coupon_name = Coupon_name
        deriving (Eq,Show)
instance SchemaType Coupon_name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_name
    schemaTypeToXML s x@Coupon_name{} =
        toXMLElement s []
            []
 
elementCoupon_name :: XMLParser Xsd.XsdString
elementCoupon_name = parseSchemaType "coupon-name"
elementToXMLCoupon_name :: Xsd.XsdString -> [Content ()]
elementToXMLCoupon_name = schemaTypeToXML "coupon-name"
 
data Coupon_value = Coupon_value
        deriving (Eq,Show)
instance SchemaType Coupon_value where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_value
    schemaTypeToXML s x@Coupon_value{} =
        toXMLElement s []
            []
 
elementCoupon_value :: XMLParser Xsd.Decimal
elementCoupon_value = parseSchemaType "coupon-value"
elementToXMLCoupon_value :: Xsd.Decimal -> [Content ()]
elementToXMLCoupon_value = schemaTypeToXML "coupon-value"
 
data Coupon_discount_type = Coupon_discount_type
        deriving (Eq,Show)
instance SchemaType Coupon_discount_type where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_discount_type
    schemaTypeToXML s x@Coupon_discount_type{} =
        toXMLElement s []
            []
 
elementCoupon_discount_type :: XMLParser Xsd.XsdString
elementCoupon_discount_type = parseSchemaType "coupon-discount-type"
elementToXMLCoupon_discount_type :: Xsd.XsdString -> [Content ()]
elementToXMLCoupon_discount_type = schemaTypeToXML "coupon-discount-type"
 
data Product_owner_id = Product_owner_id
        deriving (Eq,Show)
instance SchemaType Product_owner_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Product_owner_id
    schemaTypeToXML s x@Product_owner_id{} =
        toXMLElement s []
            []
 
elementProduct_owner_id :: XMLParser Xs.Long
elementProduct_owner_id = parseSchemaType "product-owner-id"
elementToXMLProduct_owner_id :: Xs.Long -> [Content ()]
elementToXMLProduct_owner_id = schemaTypeToXML "product-owner-id"
 
data Coupon_id = Coupon_id
        deriving (Eq,Show)
instance SchemaType Coupon_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_id
    schemaTypeToXML s x@Coupon_id{} =
        toXMLElement s []
            []
 
elementCoupon_id :: XMLParser Xs.Long
elementCoupon_id = parseSchemaType "coupon-id"
elementToXMLCoupon_id :: Xs.Long -> [Content ()]
elementToXMLCoupon_id = schemaTypeToXML "coupon-id"
 
data Coupon_display_name = Coupon_display_name
        deriving (Eq,Show)
instance SchemaType Coupon_display_name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_display_name
    schemaTypeToXML s x@Coupon_display_name{} =
        toXMLElement s []
            []
 
elementCoupon_display_name :: XMLParser Xsd.XsdString
elementCoupon_display_name = parseSchemaType "coupon-display-name"
elementToXMLCoupon_display_name :: Xsd.XsdString -> [Content ()]
elementToXMLCoupon_display_name = schemaTypeToXML "coupon-display-name"
 
data Price_discount = Price_discount
        { price_discount_currency :: Xsd.XsdString
        , price_discount_amount :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Price_discount where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Price_discount
            `apply` elementCurrency
            `apply` elementAmount
    schemaTypeToXML s x@Price_discount{} =
        toXMLElement s []
            [ elementToXMLCurrency $ price_discount_currency x
            , elementToXMLAmount $ price_discount_amount x
            ]
 
elementPrice_discount :: XMLParser Price_discount
elementPrice_discount = parseSchemaType "price-discount"
elementToXMLPrice_discount :: Price_discount -> [Content ()]
elementToXMLPrice_discount = schemaTypeToXML "price-discount"
 
data Price_discounts = Price_discounts
        { price_discounts_price_discount :: [Price_discount]
        }
        deriving (Eq,Show)
instance SchemaType Price_discounts where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Price_discounts
            `apply` many1 (elementPrice_discount)
    schemaTypeToXML s x@Price_discounts{} =
        toXMLElement s []
            [ concatMap (elementToXMLPrice_discount) $ price_discounts_price_discount x
            ]
 
elementPrice_discounts :: XMLParser Price_discounts
elementPrice_discounts = parseSchemaType "price-discounts"
elementToXMLPrice_discounts :: Price_discounts -> [Content ()]
elementToXMLPrice_discounts = schemaTypeToXML "price-discounts"
 
data Coupon_price = Coupon_price
        { coupon_price_price_discounts :: Price_discounts
        }
        deriving (Eq,Show)
instance SchemaType Coupon_price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_price
            `apply` elementPrice_discounts
    schemaTypeToXML s x@Coupon_price{} =
        toXMLElement s []
            [ elementToXMLPrice_discounts $ coupon_price_price_discounts x
            ]
 
elementCoupon_price :: XMLParser Coupon_price
elementCoupon_price = parseSchemaType "coupon-price"
elementToXMLCoupon_price :: Coupon_price -> [Content ()]
elementToXMLCoupon_price = schemaTypeToXML "coupon-price"
 
data Coupon_percent = Coupon_percent
        { coupon_percent_percent_discount :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Coupon_percent where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_percent
            `apply` elementPercent_discount
    schemaTypeToXML s x@Coupon_percent{} =
        toXMLElement s []
            [ elementToXMLPercent_discount $ coupon_percent_percent_discount x
            ]
 
elementCoupon_percent :: XMLParser Coupon_percent
elementCoupon_percent = parseSchemaType "coupon-percent"
elementToXMLCoupon_percent :: Coupon_percent -> [Content ()]
elementToXMLCoupon_percent = schemaTypeToXML "coupon-percent"
 
data Percent_discount = Percent_discount
        deriving (Eq,Show)
instance SchemaType Percent_discount where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Percent_discount
    schemaTypeToXML s x@Percent_discount{} =
        toXMLElement s []
            []
 
elementPercent_discount :: XMLParser Xsd.Decimal
elementPercent_discount = parseSchemaType "percent-discount"
elementToXMLPercent_discount :: Xsd.Decimal -> [Content ()]
elementToXMLPercent_discount = schemaTypeToXML "percent-discount"
 
data Coupon_discount_policy = Coupon_discount_policy
        { coupon_discount_policy_coupon_discount_type :: Xsd.XsdString
        , coupon_discount_policy_coupon_price :: Maybe Coupon_price
        , coupon_discount_policy_coupon_percent :: Maybe Coupon_percent
        }
        deriving (Eq,Show)
instance SchemaType Coupon_discount_policy where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon_discount_policy
            `apply` elementCoupon_discount_type
            `apply` optional (elementCoupon_price)
            `apply` optional (elementCoupon_percent)
    schemaTypeToXML s x@Coupon_discount_policy{} =
        toXMLElement s []
            [ elementToXMLCoupon_discount_type $ coupon_discount_policy_coupon_discount_type x
            , maybe [] (elementToXMLCoupon_price) $ coupon_discount_policy_coupon_price x
            , maybe [] (elementToXMLCoupon_percent) $ coupon_discount_policy_coupon_percent x
            ]
 
elementCoupon_discount_policy :: XMLParser Coupon_discount_policy
elementCoupon_discount_policy = parseSchemaType "coupon-discount-policy"
elementToXMLCoupon_discount_policy :: Coupon_discount_policy -> [Content ()]
elementToXMLCoupon_discount_policy = schemaTypeToXML "coupon-discount-policy"
 
data Credit_card_info = Credit_card_info
        { credit_card_info_billing_contact_info :: Maybe Billing_contact_info
        , credit_card_info_credit_card :: Credit_card
        }
        deriving (Eq,Show)
instance SchemaType Credit_card_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Credit_card_info
            `apply` optional (elementBilling_contact_info)
            `apply` elementCredit_card
    schemaTypeToXML s x@Credit_card_info{} =
        toXMLElement s []
            [ maybe [] (elementToXMLBilling_contact_info) $ credit_card_info_billing_contact_info x
            , elementToXMLCredit_card $ credit_card_info_credit_card x
            ]
 
elementCredit_card_info :: XMLParser Credit_card_info
elementCredit_card_info = parseSchemaType "credit-card-info"
elementToXMLCredit_card_info :: Credit_card_info -> [Content ()]
elementToXMLCredit_card_info = schemaTypeToXML "credit-card-info"
 
data Billing_contact_info = Billing_contact_info
        { billing_contact_info_first_name :: Maybe Xsd.XsdString
        , billing_contact_info_last_name :: Maybe Xsd.XsdString
        , billing_contact_info_address1 :: Maybe Xsd.XsdString
        , billing_contact_info_address2 :: Maybe Xsd.XsdString
        , billing_contact_info_city :: Maybe Xsd.XsdString
        , billing_contact_info_state :: Maybe Xsd.XsdString
        , billing_contact_info_zip :: Maybe Xsd.XsdString
        , billing_contact_info_country :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Billing_contact_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Billing_contact_info
            `apply` optional (elementFirst_name)
            `apply` optional (elementLast_name)
            `apply` optional (elementAddress1)
            `apply` optional (elementAddress2)
            `apply` optional (elementCity)
            `apply` optional (elementState)
            `apply` optional (elementZip)
            `apply` optional (elementCountry)
    schemaTypeToXML s x@Billing_contact_info{} =
        toXMLElement s []
            [ maybe [] (elementToXMLFirst_name) $ billing_contact_info_first_name x
            , maybe [] (elementToXMLLast_name) $ billing_contact_info_last_name x
            , maybe [] (elementToXMLAddress1) $ billing_contact_info_address1 x
            , maybe [] (elementToXMLAddress2) $ billing_contact_info_address2 x
            , maybe [] (elementToXMLCity) $ billing_contact_info_city x
            , maybe [] (elementToXMLState) $ billing_contact_info_state x
            , maybe [] (elementToXMLZip) $ billing_contact_info_zip x
            , maybe [] (elementToXMLCountry) $ billing_contact_info_country x
            ]
 
elementBilling_contact_info :: XMLParser Billing_contact_info
elementBilling_contact_info = parseSchemaType "billing-contact-info"
elementToXMLBilling_contact_info :: Billing_contact_info -> [Content ()]
elementToXMLBilling_contact_info = schemaTypeToXML "billing-contact-info"
 
data Credit_card = Credit_card
        { credit_card_card_number :: Maybe Xsd.XsdString
        , credit_card_encrypted_card_number :: Maybe Xsd.XsdString
        , credit_card_card_last_four_digits :: Maybe Xsd.XsdString
        , credit_card_card_type :: Xsd.XsdString
        , credit_card_expiration_month :: Maybe Xsd.XsdString
        , credit_card_expiration_year :: Maybe Xsd.XsdString
        , credit_card_security_code :: Maybe Xsd.XsdString
        , credit_card_encrypted_security_code :: Maybe Xsd.XsdString
        , credit_card_start_month :: Maybe Xsd.XsdString
        , credit_card_start_year :: Maybe Xsd.XsdString
        , credit_card_issue_number :: Maybe Xsd.XsdString
        , credit_card_card_fingerprint :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Credit_card where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Credit_card
            `apply` optional (elementCard_number)
            `apply` optional (elementEncrypted_card_number)
            `apply` optional (elementCard_last_four_digits)
            `apply` elementCard_type
            `apply` optional (elementExpiration_month)
            `apply` optional (elementExpiration_year)
            `apply` optional (elementSecurity_code)
            `apply` optional (elementEncrypted_security_code)
            `apply` optional (elementStart_month)
            `apply` optional (elementStart_year)
            `apply` optional (elementIssue_number)
            `apply` optional (elementCard_fingerprint)
    schemaTypeToXML s x@Credit_card{} =
        toXMLElement s []
            [ maybe [] (elementToXMLCard_number) $ credit_card_card_number x
            , maybe [] (elementToXMLEncrypted_card_number) $ credit_card_encrypted_card_number x
            , maybe [] (elementToXMLCard_last_four_digits) $ credit_card_card_last_four_digits x
            , elementToXMLCard_type $ credit_card_card_type x
            , maybe [] (elementToXMLExpiration_month) $ credit_card_expiration_month x
            , maybe [] (elementToXMLExpiration_year) $ credit_card_expiration_year x
            , maybe [] (elementToXMLSecurity_code) $ credit_card_security_code x
            , maybe [] (elementToXMLEncrypted_security_code) $ credit_card_encrypted_security_code x
            , maybe [] (elementToXMLStart_month) $ credit_card_start_month x
            , maybe [] (elementToXMLStart_year) $ credit_card_start_year x
            , maybe [] (elementToXMLIssue_number) $ credit_card_issue_number x
            , maybe [] (elementToXMLCard_fingerprint) $ credit_card_card_fingerprint x
            ]
 
elementCredit_card :: XMLParser Credit_card
elementCredit_card = parseSchemaType "credit-card"
elementToXMLCredit_card :: Credit_card -> [Content ()]
elementToXMLCredit_card = schemaTypeToXML "credit-card"
 
data Card_type = Card_type
        deriving (Eq,Show)
instance SchemaType Card_type where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Card_type
    schemaTypeToXML s x@Card_type{} =
        toXMLElement s []
            []
 
elementCard_type :: XMLParser Xsd.XsdString
elementCard_type = parseSchemaType "card-type"
elementToXMLCard_type :: Xsd.XsdString -> [Content ()]
elementToXMLCard_type = schemaTypeToXML "card-type"
 
data Card_number = Card_number
        deriving (Eq,Show)
instance SchemaType Card_number where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Card_number
    schemaTypeToXML s x@Card_number{} =
        toXMLElement s []
            []
 
elementCard_number :: XMLParser Xsd.XsdString
elementCard_number = parseSchemaType "card-number"
elementToXMLCard_number :: Xsd.XsdString -> [Content ()]
elementToXMLCard_number = schemaTypeToXML "card-number"
 
data Encrypted_card_number = Encrypted_card_number
        deriving (Eq,Show)
instance SchemaType Encrypted_card_number where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Encrypted_card_number
    schemaTypeToXML s x@Encrypted_card_number{} =
        toXMLElement s []
            []
 
elementEncrypted_card_number :: XMLParser Xsd.XsdString
elementEncrypted_card_number = parseSchemaType "encrypted-card-number"
elementToXMLEncrypted_card_number :: Xsd.XsdString -> [Content ()]
elementToXMLEncrypted_card_number = schemaTypeToXML "encrypted-card-number"
 
data Card_last_four_digits = Card_last_four_digits
        deriving (Eq,Show)
instance SchemaType Card_last_four_digits where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Card_last_four_digits
    schemaTypeToXML s x@Card_last_four_digits{} =
        toXMLElement s []
            []
 
elementCard_last_four_digits :: XMLParser Xsd.XsdString
elementCard_last_four_digits = parseSchemaType "card-last-four-digits"
elementToXMLCard_last_four_digits :: Xsd.XsdString -> [Content ()]
elementToXMLCard_last_four_digits = schemaTypeToXML "card-last-four-digits"
 
data Card_fingerprint = Card_fingerprint
        deriving (Eq,Show)
instance SchemaType Card_fingerprint where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Card_fingerprint
    schemaTypeToXML s x@Card_fingerprint{} =
        toXMLElement s []
            []
 
elementCard_fingerprint :: XMLParser Xsd.XsdString
elementCard_fingerprint = parseSchemaType "card-fingerprint"
elementToXMLCard_fingerprint :: Xsd.XsdString -> [Content ()]
elementToXMLCard_fingerprint = schemaTypeToXML "card-fingerprint"
 
data Security_code = Security_code
        deriving (Eq,Show)
instance SchemaType Security_code where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Security_code
    schemaTypeToXML s x@Security_code{} =
        toXMLElement s []
            []
 
elementSecurity_code :: XMLParser Xsd.XsdString
elementSecurity_code = parseSchemaType "security-code"
elementToXMLSecurity_code :: Xsd.XsdString -> [Content ()]
elementToXMLSecurity_code = schemaTypeToXML "security-code"
 
data Encrypted_security_code = Encrypted_security_code
        deriving (Eq,Show)
instance SchemaType Encrypted_security_code where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Encrypted_security_code
    schemaTypeToXML s x@Encrypted_security_code{} =
        toXMLElement s []
            []
 
elementEncrypted_security_code :: XMLParser Xsd.XsdString
elementEncrypted_security_code = parseSchemaType "encrypted-security-code"
elementToXMLEncrypted_security_code :: Xsd.XsdString -> [Content ()]
elementToXMLEncrypted_security_code = schemaTypeToXML "encrypted-security-code"
 
data Expiration_month = Expiration_month
        deriving (Eq,Show)
instance SchemaType Expiration_month where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Expiration_month
    schemaTypeToXML s x@Expiration_month{} =
        toXMLElement s []
            []
 
elementExpiration_month :: XMLParser Xsd.XsdString
elementExpiration_month = parseSchemaType "expiration-month"
elementToXMLExpiration_month :: Xsd.XsdString -> [Content ()]
elementToXMLExpiration_month = schemaTypeToXML "expiration-month"
 
data Expiration_year = Expiration_year
        deriving (Eq,Show)
instance SchemaType Expiration_year where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Expiration_year
    schemaTypeToXML s x@Expiration_year{} =
        toXMLElement s []
            []
 
elementExpiration_year :: XMLParser Xsd.XsdString
elementExpiration_year = parseSchemaType "expiration-year"
elementToXMLExpiration_year :: Xsd.XsdString -> [Content ()]
elementToXMLExpiration_year = schemaTypeToXML "expiration-year"
 
data Start_year = Start_year
        deriving (Eq,Show)
instance SchemaType Start_year where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Start_year
    schemaTypeToXML s x@Start_year{} =
        toXMLElement s []
            []
 
elementStart_year :: XMLParser Xsd.XsdString
elementStart_year = parseSchemaType "start-year"
elementToXMLStart_year :: Xsd.XsdString -> [Content ()]
elementToXMLStart_year = schemaTypeToXML "start-year"
 
data Start_month = Start_month
        deriving (Eq,Show)
instance SchemaType Start_month where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Start_month
    schemaTypeToXML s x@Start_month{} =
        toXMLElement s []
            []
 
elementStart_month :: XMLParser Xsd.XsdString
elementStart_month = parseSchemaType "start-month"
elementToXMLStart_month :: Xsd.XsdString -> [Content ()]
elementToXMLStart_month = schemaTypeToXML "start-month"
 
data Issue_number = Issue_number
        deriving (Eq,Show)
instance SchemaType Issue_number where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Issue_number
    schemaTypeToXML s x@Issue_number{} =
        toXMLElement s []
            []
 
elementIssue_number :: XMLParser Xsd.XsdString
elementIssue_number = parseSchemaType "issue-number"
elementToXMLIssue_number :: Xsd.XsdString -> [Content ()]
elementToXMLIssue_number = schemaTypeToXML "issue-number"
 
data Custom_parameter = Custom_parameter
        { custom_parameter_custom_param_id :: Maybe Xs.Long
        , custom_parameter_custom_param_title :: Xsd.XsdString
        , custom_parameter_active :: Xsd.Boolean
        , custom_parameter_mandatory :: Xsd.Boolean
        , custom_parameter_custom_param_settings :: Custom_param_settings
        }
        deriving (Eq,Show)
instance SchemaType Custom_parameter where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Custom_parameter
            `apply` optional (elementCustom_param_id)
            `apply` elementCustom_param_title
            `apply` elementActive
            `apply` elementMandatory
            `apply` elementCustom_param_settings
    schemaTypeToXML s x@Custom_parameter{} =
        toXMLElement s []
            [ maybe [] (elementToXMLCustom_param_id) $ custom_parameter_custom_param_id x
            , elementToXMLCustom_param_title $ custom_parameter_custom_param_title x
            , elementToXMLActive $ custom_parameter_active x
            , elementToXMLMandatory $ custom_parameter_mandatory x
            , elementToXMLCustom_param_settings $ custom_parameter_custom_param_settings x
            ]
 
elementCustom_parameter :: XMLParser Custom_parameter
elementCustom_parameter = parseSchemaType "custom-parameter"
elementToXMLCustom_parameter :: Custom_parameter -> [Content ()]
elementToXMLCustom_parameter = schemaTypeToXML "custom-parameter"
 
data Custom_param_settings = Custom_param_settings
        { custom_param_settings_custom_param_type :: Xsd.XsdString
        , custom_param_settings_short_text_type :: Short_text_type
        , custom_param_settings_long_text_type :: Long_text_type
        , custom_param_settings_fixed_selection_type :: Fixed_selection_type
        , custom_param_settings_merchant_value_type :: Merchant_value_type
        , custom_param_settings_visual_settings :: Visual_settings
        }
        deriving (Eq,Show)
instance SchemaType Custom_param_settings where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Custom_param_settings
            `apply` elementCustom_param_type
            `apply` elementShort_text_type
            `apply` elementLong_text_type
            `apply` elementFixed_selection_type
            `apply` elementMerchant_value_type
            `apply` elementVisual_settings
    schemaTypeToXML s x@Custom_param_settings{} =
        toXMLElement s []
            [ elementToXMLCustom_param_type $ custom_param_settings_custom_param_type x
            , elementToXMLShort_text_type $ custom_param_settings_short_text_type x
            , elementToXMLLong_text_type $ custom_param_settings_long_text_type x
            , elementToXMLFixed_selection_type $ custom_param_settings_fixed_selection_type x
            , elementToXMLMerchant_value_type $ custom_param_settings_merchant_value_type x
            , elementToXMLVisual_settings $ custom_param_settings_visual_settings x
            ]
 
elementCustom_param_settings :: XMLParser Custom_param_settings
elementCustom_param_settings = parseSchemaType "custom-param-settings"
elementToXMLCustom_param_settings :: Custom_param_settings -> [Content ()]
elementToXMLCustom_param_settings = schemaTypeToXML "custom-param-settings"
 
data Short_text_type = Short_text_type
        { short_text_type_minimum_length :: Xsd.Decimal
        , short_text_type_maximum_length :: Xsd.Decimal
        , short_text_type_regex_validation :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Short_text_type where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Short_text_type
            `apply` elementMinimum_length
            `apply` elementMaximum_length
            `apply` elementRegex_validation
    schemaTypeToXML s x@Short_text_type{} =
        toXMLElement s []
            [ elementToXMLMinimum_length $ short_text_type_minimum_length x
            , elementToXMLMaximum_length $ short_text_type_maximum_length x
            , elementToXMLRegex_validation $ short_text_type_regex_validation x
            ]
 
elementShort_text_type :: XMLParser Short_text_type
elementShort_text_type = parseSchemaType "short-text-type"
elementToXMLShort_text_type :: Short_text_type -> [Content ()]
elementToXMLShort_text_type = schemaTypeToXML "short-text-type"
 
data Long_text_type = Long_text_type
        { long_text_type_minimum_length :: Xsd.Decimal
        , long_text_type_maximum_length :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Long_text_type where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Long_text_type
            `apply` elementMinimum_length
            `apply` elementMaximum_length
    schemaTypeToXML s x@Long_text_type{} =
        toXMLElement s []
            [ elementToXMLMinimum_length $ long_text_type_minimum_length x
            , elementToXMLMaximum_length $ long_text_type_maximum_length x
            ]
 
elementLong_text_type :: XMLParser Long_text_type
elementLong_text_type = parseSchemaType "long-text-type"
elementToXMLLong_text_type :: Long_text_type -> [Content ()]
elementToXMLLong_text_type = schemaTypeToXML "long-text-type"
 
data Fixed_selection_type = Fixed_selection_type
        { fixed_selection_type_drop_down_values :: Drop_down_values
        }
        deriving (Eq,Show)
instance SchemaType Fixed_selection_type where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Fixed_selection_type
            `apply` elementDrop_down_values
    schemaTypeToXML s x@Fixed_selection_type{} =
        toXMLElement s []
            [ elementToXMLDrop_down_values $ fixed_selection_type_drop_down_values x
            ]
 
elementFixed_selection_type :: XMLParser Fixed_selection_type
elementFixed_selection_type = parseSchemaType "fixed-selection-type"
elementToXMLFixed_selection_type :: Fixed_selection_type -> [Content ()]
elementToXMLFixed_selection_type = schemaTypeToXML "fixed-selection-type"
 
data Merchant_value_type = Merchant_value_type
        { merchant_value_type_minimum_length :: Xsd.Decimal
        , merchant_value_type_maximum_length :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Merchant_value_type where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Merchant_value_type
            `apply` elementMinimum_length
            `apply` elementMaximum_length
    schemaTypeToXML s x@Merchant_value_type{} =
        toXMLElement s []
            [ elementToXMLMinimum_length $ merchant_value_type_minimum_length x
            , elementToXMLMaximum_length $ merchant_value_type_maximum_length x
            ]
 
elementMerchant_value_type :: XMLParser Merchant_value_type
elementMerchant_value_type = parseSchemaType "merchant-value-type"
elementToXMLMerchant_value_type :: Merchant_value_type -> [Content ()]
elementToXMLMerchant_value_type = schemaTypeToXML "merchant-value-type"
 
data Visual_settings = Visual_settings
        { visual_settings_title_alignment :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Visual_settings where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Visual_settings
            `apply` elementTitle_alignment
    schemaTypeToXML s x@Visual_settings{} =
        toXMLElement s []
            [ elementToXMLTitle_alignment $ visual_settings_title_alignment x
            ]
 
elementVisual_settings :: XMLParser Visual_settings
elementVisual_settings = parseSchemaType "visual-settings"
elementToXMLVisual_settings :: Visual_settings -> [Content ()]
elementToXMLVisual_settings = schemaTypeToXML "visual-settings"
 
data Drop_down_values = Drop_down_values
        { drop_down_values_drop_down_value :: [Xsd.XsdString]
        }
        deriving (Eq,Show)
instance SchemaType Drop_down_values where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Drop_down_values
            `apply` many (elementDrop_down_value)
    schemaTypeToXML s x@Drop_down_values{} =
        toXMLElement s []
            [ concatMap (elementToXMLDrop_down_value) $ drop_down_values_drop_down_value x
            ]
 
elementDrop_down_values :: XMLParser Drop_down_values
elementDrop_down_values = parseSchemaType "drop-down-values"
elementToXMLDrop_down_values :: Drop_down_values -> [Content ()]
elementToXMLDrop_down_values = schemaTypeToXML "drop-down-values"
 
data Drop_down_value = Drop_down_value
        deriving (Eq,Show)
instance SchemaType Drop_down_value where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Drop_down_value
    schemaTypeToXML s x@Drop_down_value{} =
        toXMLElement s []
            []
 
elementDrop_down_value :: XMLParser Xsd.XsdString
elementDrop_down_value = parseSchemaType "drop-down-value"
elementToXMLDrop_down_value :: Xsd.XsdString -> [Content ()]
elementToXMLDrop_down_value = schemaTypeToXML "drop-down-value"
 
data Custom_param_id = Custom_param_id
        deriving (Eq,Show)
instance SchemaType Custom_param_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Custom_param_id
    schemaTypeToXML s x@Custom_param_id{} =
        toXMLElement s []
            []
 
elementCustom_param_id :: XMLParser Xs.Long
elementCustom_param_id = parseSchemaType "custom-param-id"
elementToXMLCustom_param_id :: Xs.Long -> [Content ()]
elementToXMLCustom_param_id = schemaTypeToXML "custom-param-id"
 
data Custom_param_title = Custom_param_title
        deriving (Eq,Show)
instance SchemaType Custom_param_title where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Custom_param_title
    schemaTypeToXML s x@Custom_param_title{} =
        toXMLElement s []
            []
 
elementCustom_param_title :: XMLParser Xsd.XsdString
elementCustom_param_title = parseSchemaType "custom-param-title"
elementToXMLCustom_param_title :: Xsd.XsdString -> [Content ()]
elementToXMLCustom_param_title = schemaTypeToXML "custom-param-title"
 
data Custom_param_type = Custom_param_type
        deriving (Eq,Show)
instance SchemaType Custom_param_type where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Custom_param_type
    schemaTypeToXML s x@Custom_param_type{} =
        toXMLElement s []
            []
 
elementCustom_param_type :: XMLParser Xsd.XsdString
elementCustom_param_type = parseSchemaType "custom-param-type"
elementToXMLCustom_param_type :: Xsd.XsdString -> [Content ()]
elementToXMLCustom_param_type = schemaTypeToXML "custom-param-type"
 
data Mandatory = Mandatory
        deriving (Eq,Show)
instance SchemaType Mandatory where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Mandatory
    schemaTypeToXML s x@Mandatory{} =
        toXMLElement s []
            []
 
elementMandatory :: XMLParser Xsd.Boolean
elementMandatory = parseSchemaType "mandatory"
elementToXMLMandatory :: Xsd.Boolean -> [Content ()]
elementToXMLMandatory = schemaTypeToXML "mandatory"
 
data Custom_param_instructions = Custom_param_instructions
        deriving (Eq,Show)
instance SchemaType Custom_param_instructions where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Custom_param_instructions
    schemaTypeToXML s x@Custom_param_instructions{} =
        toXMLElement s []
            []
 
elementCustom_param_instructions :: XMLParser Xsd.XsdString
elementCustom_param_instructions = parseSchemaType "custom-param-instructions"
elementToXMLCustom_param_instructions :: Xsd.XsdString -> [Content ()]
elementToXMLCustom_param_instructions = schemaTypeToXML "custom-param-instructions"
 
data Title_alignment = Title_alignment
        deriving (Eq,Show)
instance SchemaType Title_alignment where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Title_alignment
    schemaTypeToXML s x@Title_alignment{} =
        toXMLElement s []
            []
 
elementTitle_alignment :: XMLParser Xsd.XsdString
elementTitle_alignment = parseSchemaType "title-alignment"
elementToXMLTitle_alignment :: Xsd.XsdString -> [Content ()]
elementToXMLTitle_alignment = schemaTypeToXML "title-alignment"
 
data Minimum_length = Minimum_length
        deriving (Eq,Show)
instance SchemaType Minimum_length where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Minimum_length
    schemaTypeToXML s x@Minimum_length{} =
        toXMLElement s []
            []
 
elementMinimum_length :: XMLParser Xsd.Decimal
elementMinimum_length = parseSchemaType "minimum-length"
elementToXMLMinimum_length :: Xsd.Decimal -> [Content ()]
elementToXMLMinimum_length = schemaTypeToXML "minimum-length"
 
data Maximum_length = Maximum_length
        deriving (Eq,Show)
instance SchemaType Maximum_length where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Maximum_length
    schemaTypeToXML s x@Maximum_length{} =
        toXMLElement s []
            []
 
elementMaximum_length :: XMLParser Xsd.Decimal
elementMaximum_length = parseSchemaType "maximum-length"
elementToXMLMaximum_length :: Xsd.Decimal -> [Content ()]
elementToXMLMaximum_length = schemaTypeToXML "maximum-length"
 
data Regex_validation = Regex_validation
        deriving (Eq,Show)
instance SchemaType Regex_validation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Regex_validation
    schemaTypeToXML s x@Regex_validation{} =
        toXMLElement s []
            []
 
elementRegex_validation :: XMLParser Xsd.XsdString
elementRegex_validation = parseSchemaType "regex-validation"
elementToXMLRegex_validation :: Xsd.XsdString -> [Content ()]
elementToXMLRegex_validation = schemaTypeToXML "regex-validation"
 
data Active = Active
        deriving (Eq,Show)
instance SchemaType Active where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Active
    schemaTypeToXML s x@Active{} =
        toXMLElement s []
            []
 
elementActive :: XMLParser Xsd.Boolean
elementActive = parseSchemaType "active"
elementToXMLActive :: Xsd.Boolean -> [Content ()]
elementToXMLActive = schemaTypeToXML "active"
 
data Ecps_info = Ecps_info
        { ecps_info_ecp_info :: [Ecp_info]
        }
        deriving (Eq,Show)
instance SchemaType Ecps_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Ecps_info
            `apply` many (elementEcp_info)
    schemaTypeToXML s x@Ecps_info{} =
        toXMLElement s []
            [ concatMap (elementToXMLEcp_info) $ ecps_info_ecp_info x
            ]
 
elementEcps_info :: XMLParser Ecps_info
elementEcps_info = parseSchemaType "ecps-info"
elementToXMLEcps_info :: Ecps_info -> [Content ()]
elementToXMLEcps_info = schemaTypeToXML "ecps-info"
 
data Ecp_info = Ecp_info
        { ecp_info_billing_contact_info :: Billing_contact_info
        , ecp_info_ecp :: Ecp
        }
        deriving (Eq,Show)
instance SchemaType Ecp_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Ecp_info
            `apply` elementBilling_contact_info
            `apply` elementEcp
    schemaTypeToXML s x@Ecp_info{} =
        toXMLElement s []
            [ elementToXMLBilling_contact_info $ ecp_info_billing_contact_info x
            , elementToXMLEcp $ ecp_info_ecp x
            ]
 
elementEcp_info :: XMLParser Ecp_info
elementEcp_info = parseSchemaType "ecp-info"
elementToXMLEcp_info :: Ecp_info -> [Content ()]
elementToXMLEcp_info = schemaTypeToXML "ecp-info"
 
data Ecp = Ecp
        { ecp_account_number :: Xsd.XsdString
        , ecp_routing_number :: Xsd.XsdString
        , ecp_account_type :: Xsd.XsdString
        , ecp_public_account_number :: Xsd.XsdString
        , ecp_public_routing_number :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Ecp where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Ecp
            `apply` elementAccount_number
            `apply` elementRouting_number
            `apply` elementAccount_type
            `apply` elementPublic_account_number
            `apply` elementPublic_routing_number
    schemaTypeToXML s x@Ecp{} =
        toXMLElement s []
            [ elementToXMLAccount_number $ ecp_account_number x
            , elementToXMLRouting_number $ ecp_routing_number x
            , elementToXMLAccount_type $ ecp_account_type x
            , elementToXMLPublic_account_number $ ecp_public_account_number x
            , elementToXMLPublic_routing_number $ ecp_public_routing_number x
            ]
 
elementEcp :: XMLParser Ecp
elementEcp = parseSchemaType "ecp"
elementToXMLEcp :: Ecp -> [Content ()]
elementToXMLEcp = schemaTypeToXML "ecp"
 
data Account_number = Account_number
        deriving (Eq,Show)
instance SchemaType Account_number where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Account_number
    schemaTypeToXML s x@Account_number{} =
        toXMLElement s []
            []
 
elementAccount_number :: XMLParser Xsd.XsdString
elementAccount_number = parseSchemaType "account-number"
elementToXMLAccount_number :: Xsd.XsdString -> [Content ()]
elementToXMLAccount_number = schemaTypeToXML "account-number"
 
data Account_type = Account_type
        deriving (Eq,Show)
instance SchemaType Account_type where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Account_type
    schemaTypeToXML s x@Account_type{} =
        toXMLElement s []
            []
 
elementAccount_type :: XMLParser Xsd.XsdString
elementAccount_type = parseSchemaType "account-type"
elementToXMLAccount_type :: Xsd.XsdString -> [Content ()]
elementToXMLAccount_type = schemaTypeToXML "account-type"
 
data Routing_number = Routing_number
        deriving (Eq,Show)
instance SchemaType Routing_number where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Routing_number
    schemaTypeToXML s x@Routing_number{} =
        toXMLElement s []
            []
 
elementRouting_number :: XMLParser Xsd.XsdString
elementRouting_number = parseSchemaType "routing-number"
elementToXMLRouting_number :: Xsd.XsdString -> [Content ()]
elementToXMLRouting_number = schemaTypeToXML "routing-number"
 
data Public_account_number = Public_account_number
        deriving (Eq,Show)
instance SchemaType Public_account_number where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Public_account_number
    schemaTypeToXML s x@Public_account_number{} =
        toXMLElement s []
            []
 
elementPublic_account_number :: XMLParser Xsd.XsdString
elementPublic_account_number = parseSchemaType "public-account-number"
elementToXMLPublic_account_number :: Xsd.XsdString -> [Content ()]
elementToXMLPublic_account_number = schemaTypeToXML "public-account-number"
 
data Public_routing_number = Public_routing_number
        deriving (Eq,Show)
instance SchemaType Public_routing_number where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Public_routing_number
    schemaTypeToXML s x@Public_routing_number{} =
        toXMLElement s []
            []
 
elementPublic_routing_number :: XMLParser Xsd.XsdString
elementPublic_routing_number = parseSchemaType "public-routing-number"
elementToXMLPublic_routing_number :: Xsd.XsdString -> [Content ()]
elementToXMLPublic_routing_number = schemaTypeToXML "public-routing-number"
 
data Sku_license = Sku_license
        { sku_license_sku_id :: Xs.Long
        , sku_license_sku_name :: Xsd.XsdString
        , sku_license_license_key :: [Xsd.XsdString]
        }
        deriving (Eq,Show)
instance SchemaType Sku_license where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_license
            `apply` elementSku_id
            `apply` elementSku_name
            `apply` many1 (elementLicense_key)
    schemaTypeToXML s x@Sku_license{} =
        toXMLElement s []
            [ elementToXMLSku_id $ sku_license_sku_id x
            , elementToXMLSku_name $ sku_license_sku_name x
            , concatMap (elementToXMLLicense_key) $ sku_license_license_key x
            ]
 
elementSku_license :: XMLParser Sku_license
elementSku_license = parseSchemaType "sku-license"
elementToXMLSku_license :: Sku_license -> [Content ()]
elementToXMLSku_license = schemaTypeToXML "sku-license"
 
data Sku_download_link = Sku_download_link
        { sku_download_link_sku_id :: Xs.Long
        , sku_download_link_sku_name :: Xsd.XsdString
        , sku_download_link_download_link :: [Xsd.XsdString]
        }
        deriving (Eq,Show)
instance SchemaType Sku_download_link where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_download_link
            `apply` elementSku_id
            `apply` elementSku_name
            `apply` between (Occurs (Just 1) (Just 1))
                            (elementDownload_link)
    schemaTypeToXML s x@Sku_download_link{} =
        toXMLElement s []
            [ elementToXMLSku_id $ sku_download_link_sku_id x
            , elementToXMLSku_name $ sku_download_link_sku_name x
            , concatMap (elementToXMLDownload_link) $ sku_download_link_download_link x
            ]
 
elementSku_download_link :: XMLParser Sku_download_link
elementSku_download_link = parseSchemaType "sku-download-link"
elementToXMLSku_download_link :: Sku_download_link -> [Content ()]
elementToXMLSku_download_link = schemaTypeToXML "sku-download-link"
 
data License_keys = License_keys
        { license_keys_sku_license :: [Sku_license]
        }
        deriving (Eq,Show)
instance SchemaType License_keys where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return License_keys
            `apply` many (elementSku_license)
    schemaTypeToXML s x@License_keys{} =
        toXMLElement s []
            [ concatMap (elementToXMLSku_license) $ license_keys_sku_license x
            ]
 
elementLicense_keys :: XMLParser License_keys
elementLicense_keys = parseSchemaType "license-keys"
elementToXMLLicense_keys :: License_keys -> [Content ()]
elementToXMLLicense_keys = schemaTypeToXML "license-keys"
 
data Download_links = Download_links
        { download_links_sku_download_link :: [Sku_download_link]
        }
        deriving (Eq,Show)
instance SchemaType Download_links where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Download_links
            `apply` many (elementSku_download_link)
    schemaTypeToXML s x@Download_links{} =
        toXMLElement s []
            [ concatMap (elementToXMLSku_download_link) $ download_links_sku_download_link x
            ]
 
elementDownload_links :: XMLParser Download_links
elementDownload_links = parseSchemaType "download-links"
elementToXMLDownload_links :: Download_links -> [Content ()]
elementToXMLDownload_links = schemaTypeToXML "download-links"
 
data Fulfillment = Fulfillment
        { fulfillment_license_keys :: [License_keys]
        , fulfillment_download_links :: [Download_links]
        }
        deriving (Eq,Show)
instance SchemaType Fulfillment where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Fulfillment
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementLicense_keys)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementDownload_links)
    schemaTypeToXML s x@Fulfillment{} =
        toXMLElement s []
            [ concatMap (elementToXMLLicense_keys) $ fulfillment_license_keys x
            , concatMap (elementToXMLDownload_links) $ fulfillment_download_links x
            ]
 
elementFulfillment :: XMLParser Fulfillment
elementFulfillment = parseSchemaType "fulfillment"
elementToXMLFulfillment :: Fulfillment -> [Content ()]
elementToXMLFulfillment = schemaTypeToXML "fulfillment"
 
data License_key = License_key
        deriving (Eq,Show)
instance SchemaType License_key where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return License_key
    schemaTypeToXML s x@License_key{} =
        toXMLElement s []
            []
 
elementLicense_key :: XMLParser Xsd.XsdString
elementLicense_key = parseSchemaType "license-key"
elementToXMLLicense_key :: Xsd.XsdString -> [Content ()]
elementToXMLLicense_key = schemaTypeToXML "license-key"
 
data Download_link = Download_link
        deriving (Eq,Show)
instance SchemaType Download_link where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Download_link
    schemaTypeToXML s x@Download_link{} =
        toXMLElement s []
            []
 
elementDownload_link :: XMLParser Xsd.XsdString
elementDownload_link = parseSchemaType "download-link"
elementToXMLDownload_link :: Xsd.XsdString -> [Content ()]
elementToXMLDownload_link = schemaTypeToXML "download-link"
 
data Store_id = Store_id
        deriving (Eq,Show)
instance SchemaType Store_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Store_id
    schemaTypeToXML s x@Store_id{} =
        toXMLElement s []
            []
 
elementStore_id :: XMLParser Xs.Long
elementStore_id = parseSchemaType "store-id"
elementToXMLStore_id :: Xs.Long -> [Content ()]
elementToXMLStore_id = schemaTypeToXML "store-id"
 
data Quantity = Quantity
        deriving (Eq,Show)
instance SchemaType Quantity where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Quantity
    schemaTypeToXML s x@Quantity{} =
        toXMLElement s []
            []
 
elementQuantity :: XMLParser Xs.Int
elementQuantity = parseSchemaType "quantity"
elementToXMLQuantity :: Xs.Int -> [Content ()]
elementToXMLQuantity = schemaTypeToXML "quantity"
 
elementTax_included :: XMLParser Xsd.Boolean
elementTax_included = parseSchemaType "tax-included"
elementToXMLTax_included :: Xsd.Boolean -> [Content ()]
elementToXMLTax_included = schemaTypeToXML "tax-included"
 
data Item_price = Item_price
        { item_price_store_id :: Xs.Long
        , item_price_quantity :: Xs.Int
        , item_price_unit_price :: Unit_price
        , item_price_total_price :: Total_price
        , item_price_total_price_with_tax :: Total_price_with_tax
        , item_price_tax :: Xsd.Decimal
        , item_price_tax_recurring :: Maybe Xsd.Decimal
        , item_price_tax_rate :: Xsd.Decimal
        , item_price_country :: Xsd.XsdString
        , item_price_state :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Item_price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Item_price
            `apply` elementStore_id
            `apply` elementQuantity
            `apply` elementUnit_price
            `apply` elementTotal_price
            `apply` elementTotal_price_with_tax
            `apply` elementTax
            `apply` optional (elementTax_recurring)
            `apply` elementTax_rate
            `apply` elementCountry
            `apply` elementState
    schemaTypeToXML s x@Item_price{} =
        toXMLElement s []
            [ elementToXMLStore_id $ item_price_store_id x
            , elementToXMLQuantity $ item_price_quantity x
            , elementToXMLUnit_price $ item_price_unit_price x
            , elementToXMLTotal_price $ item_price_total_price x
            , elementToXMLTotal_price_with_tax $ item_price_total_price_with_tax x
            , elementToXMLTax $ item_price_tax x
            , maybe [] (elementToXMLTax_recurring) $ item_price_tax_recurring x
            , elementToXMLTax_rate $ item_price_tax_rate x
            , elementToXMLCountry $ item_price_country x
            , elementToXMLState $ item_price_state x
            ]
 
elementItem_price :: XMLParser Item_price
elementItem_price = parseSchemaType "item-price"
elementToXMLItem_price :: Item_price -> [Content ()]
elementToXMLItem_price = schemaTypeToXML "item-price"
 
data Unit_price = Unit_price
        { unit_price_charge_price :: [Charge_price]
        }
        deriving (Eq,Show)
instance SchemaType Unit_price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Unit_price
            `apply` many (elementCharge_price)
    schemaTypeToXML s x@Unit_price{} =
        toXMLElement s []
            [ concatMap (elementToXMLCharge_price) $ unit_price_charge_price x
            ]
 
elementUnit_price :: XMLParser Unit_price
elementUnit_price = parseSchemaType "unit-price"
elementToXMLUnit_price :: Unit_price -> [Content ()]
elementToXMLUnit_price = schemaTypeToXML "unit-price"
 
data Total_price = Total_price
        { total_price_charge_price :: [Charge_price]
        }
        deriving (Eq,Show)
instance SchemaType Total_price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Total_price
            `apply` many (elementCharge_price)
    schemaTypeToXML s x@Total_price{} =
        toXMLElement s []
            [ concatMap (elementToXMLCharge_price) $ total_price_charge_price x
            ]
 
elementTotal_price :: XMLParser Total_price
elementTotal_price = parseSchemaType "total-price"
elementToXMLTotal_price :: Total_price -> [Content ()]
elementToXMLTotal_price = schemaTypeToXML "total-price"
 
data Total_price_with_tax = Total_price_with_tax
        { total_price_with_tax_charge_price :: [Charge_price]
        }
        deriving (Eq,Show)
instance SchemaType Total_price_with_tax where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Total_price_with_tax
            `apply` many (elementCharge_price)
    schemaTypeToXML s x@Total_price_with_tax{} =
        toXMLElement s []
            [ concatMap (elementToXMLCharge_price) $ total_price_with_tax_charge_price x
            ]
 
elementTotal_price_with_tax :: XMLParser Total_price_with_tax
elementTotal_price_with_tax = parseSchemaType "total-price-with-tax"
elementToXMLTotal_price_with_tax :: Total_price_with_tax -> [Content ()]
elementToXMLTotal_price_with_tax = schemaTypeToXML "total-price-with-tax"
 
data Charge_price = Charge_price
        { charge_price_charge_type :: Xsd.XsdString
        , charge_price_value :: Xsd.Decimal
        , charge_price_currency :: Xsd.XsdString
        , charge_price_formatted_price :: Formatted_price
        , charge_price_tax_included :: Xsd.Boolean
        }
        deriving (Eq,Show)
instance SchemaType Charge_price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Charge_price
            `apply` elementCharge_type
            `apply` elementValue
            `apply` elementCurrency
            `apply` elementFormatted_price
            `apply` elementTax_included
    schemaTypeToXML s x@Charge_price{} =
        toXMLElement s []
            [ elementToXMLCharge_type $ charge_price_charge_type x
            , elementToXMLValue $ charge_price_value x
            , elementToXMLCurrency $ charge_price_currency x
            , elementToXMLFormatted_price $ charge_price_formatted_price x
            , elementToXMLTax_included $ charge_price_tax_included x
            ]
 
elementCharge_price :: XMLParser Charge_price
elementCharge_price = parseSchemaType "charge-price"
elementToXMLCharge_price :: Charge_price -> [Content ()]
elementToXMLCharge_price = schemaTypeToXML "charge-price"
 
data Charge_type = Charge_type
        deriving (Eq,Show)
instance SchemaType Charge_type where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Charge_type
    schemaTypeToXML s x@Charge_type{} =
        toXMLElement s []
            []
 
elementCharge_type :: XMLParser Xsd.XsdString
elementCharge_type = parseSchemaType "charge-type"
elementToXMLCharge_type :: Xsd.XsdString -> [Content ()]
elementToXMLCharge_type = schemaTypeToXML "charge-type"
 
data Local_bank_transfer_info = Local_bank_transfer_info
        { local_bank_transfer_info_swift_code :: Xsd.XsdString
        , local_bank_transfer_info_payment_reference :: Xsd.XsdString
        , local_bank_transfer_info_additional_reference :: Xsd.XsdString
        , local_bank_transfer_info_account_holder :: Xsd.XsdString
        , local_bank_transfer_info_bank_name :: Xsd.XsdString
        , local_bank_transfer_info_iban :: Xsd.XsdString
        , local_bank_transfer_info_country_description :: Xsd.XsdString
        , local_bank_transfer_info_status_id :: Maybe Xs.Int
        , local_bank_transfer_info_bank_city :: Xsd.XsdString
        , local_bank_transfer_info_bank_account_number :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Local_bank_transfer_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Local_bank_transfer_info
            `apply` elementSwift_code
            `apply` elementPayment_reference
            `apply` elementAdditional_reference
            `apply` elementAccount_holder
            `apply` elementBank_name
            `apply` elementIban
            `apply` elementCountry_description
            `apply` optional (elementStatus_id)
            `apply` elementBank_city
            `apply` elementBank_account_number
    schemaTypeToXML s x@Local_bank_transfer_info{} =
        toXMLElement s []
            [ elementToXMLSwift_code $ local_bank_transfer_info_swift_code x
            , elementToXMLPayment_reference $ local_bank_transfer_info_payment_reference x
            , elementToXMLAdditional_reference $ local_bank_transfer_info_additional_reference x
            , elementToXMLAccount_holder $ local_bank_transfer_info_account_holder x
            , elementToXMLBank_name $ local_bank_transfer_info_bank_name x
            , elementToXMLIban $ local_bank_transfer_info_iban x
            , elementToXMLCountry_description $ local_bank_transfer_info_country_description x
            , maybe [] (elementToXMLStatus_id) $ local_bank_transfer_info_status_id x
            , elementToXMLBank_city $ local_bank_transfer_info_bank_city x
            , elementToXMLBank_account_number $ local_bank_transfer_info_bank_account_number x
            ]
 
elementLocal_bank_transfer_info :: XMLParser Local_bank_transfer_info
elementLocal_bank_transfer_info = parseSchemaType "local-bank-transfer-info"
elementToXMLLocal_bank_transfer_info :: Local_bank_transfer_info -> [Content ()]
elementToXMLLocal_bank_transfer_info = schemaTypeToXML "local-bank-transfer-info"
 
data Swift_code = Swift_code
        deriving (Eq,Show)
instance SchemaType Swift_code where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Swift_code
    schemaTypeToXML s x@Swift_code{} =
        toXMLElement s []
            []
 
elementSwift_code :: XMLParser Xsd.XsdString
elementSwift_code = parseSchemaType "swift-code"
elementToXMLSwift_code :: Xsd.XsdString -> [Content ()]
elementToXMLSwift_code = schemaTypeToXML "swift-code"
 
data Payment_reference = Payment_reference
        deriving (Eq,Show)
instance SchemaType Payment_reference where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Payment_reference
    schemaTypeToXML s x@Payment_reference{} =
        toXMLElement s []
            []
 
elementPayment_reference :: XMLParser Xsd.XsdString
elementPayment_reference = parseSchemaType "payment-reference"
elementToXMLPayment_reference :: Xsd.XsdString -> [Content ()]
elementToXMLPayment_reference = schemaTypeToXML "payment-reference"
 
data Additional_reference = Additional_reference
        deriving (Eq,Show)
instance SchemaType Additional_reference where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Additional_reference
    schemaTypeToXML s x@Additional_reference{} =
        toXMLElement s []
            []
 
elementAdditional_reference :: XMLParser Xsd.XsdString
elementAdditional_reference = parseSchemaType "additional-reference"
elementToXMLAdditional_reference :: Xsd.XsdString -> [Content ()]
elementToXMLAdditional_reference = schemaTypeToXML "additional-reference"
 
data Account_holder = Account_holder
        deriving (Eq,Show)
instance SchemaType Account_holder where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Account_holder
    schemaTypeToXML s x@Account_holder{} =
        toXMLElement s []
            []
 
elementAccount_holder :: XMLParser Xsd.XsdString
elementAccount_holder = parseSchemaType "account-holder"
elementToXMLAccount_holder :: Xsd.XsdString -> [Content ()]
elementToXMLAccount_holder = schemaTypeToXML "account-holder"
 
data Bank_name = Bank_name
        deriving (Eq,Show)
instance SchemaType Bank_name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Bank_name
    schemaTypeToXML s x@Bank_name{} =
        toXMLElement s []
            []
 
elementBank_name :: XMLParser Xsd.XsdString
elementBank_name = parseSchemaType "bank-name"
elementToXMLBank_name :: Xsd.XsdString -> [Content ()]
elementToXMLBank_name = schemaTypeToXML "bank-name"
 
data Effort_id = Effort_id
        deriving (Eq,Show)
instance SchemaType Effort_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Effort_id
    schemaTypeToXML s x@Effort_id{} =
        toXMLElement s []
            []
 
elementEffort_id :: XMLParser Xs.Int
elementEffort_id = parseSchemaType "effort-id"
elementToXMLEffort_id :: Xs.Int -> [Content ()]
elementToXMLEffort_id = schemaTypeToXML "effort-id"
 
data Attempt_id = Attempt_id
        deriving (Eq,Show)
instance SchemaType Attempt_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Attempt_id
    schemaTypeToXML s x@Attempt_id{} =
        toXMLElement s []
            []
 
elementAttempt_id :: XMLParser Xs.Int
elementAttempt_id = parseSchemaType "attempt-id"
elementToXMLAttempt_id :: Xs.Int -> [Content ()]
elementToXMLAttempt_id = schemaTypeToXML "attempt-id"
 
data Iban = Iban
        deriving (Eq,Show)
instance SchemaType Iban where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Iban
    schemaTypeToXML s x@Iban{} =
        toXMLElement s []
            []
 
elementIban :: XMLParser Xsd.XsdString
elementIban = parseSchemaType "iban"
elementToXMLIban :: Xsd.XsdString -> [Content ()]
elementToXMLIban = schemaTypeToXML "iban"
 
data Country_description = Country_description
        deriving (Eq,Show)
instance SchemaType Country_description where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Country_description
    schemaTypeToXML s x@Country_description{} =
        toXMLElement s []
            []
 
elementCountry_description :: XMLParser Xsd.XsdString
elementCountry_description = parseSchemaType "country-description"
elementToXMLCountry_description :: Xsd.XsdString -> [Content ()]
elementToXMLCountry_description = schemaTypeToXML "country-description"
 
data Bank_city = Bank_city
        deriving (Eq,Show)
instance SchemaType Bank_city where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Bank_city
    schemaTypeToXML s x@Bank_city{} =
        toXMLElement s []
            []
 
elementBank_city :: XMLParser Xsd.XsdString
elementBank_city = parseSchemaType "bank-city"
elementToXMLBank_city :: Xsd.XsdString -> [Content ()]
elementToXMLBank_city = schemaTypeToXML "bank-city"
 
data Status_id = Status_id
        deriving (Eq,Show)
instance SchemaType Status_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Status_id
    schemaTypeToXML s x@Status_id{} =
        toXMLElement s []
            []
 
elementStatus_id :: XMLParser Xs.Int
elementStatus_id = parseSchemaType "status-id"
elementToXMLStatus_id :: Xs.Int -> [Content ()]
elementToXMLStatus_id = schemaTypeToXML "status-id"
 
data Bank_account_number = Bank_account_number
        deriving (Eq,Show)
instance SchemaType Bank_account_number where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Bank_account_number
    schemaTypeToXML s x@Bank_account_number{} =
        toXMLElement s []
            []
 
elementBank_account_number :: XMLParser Xsd.XsdString
elementBank_account_number = parseSchemaType "bank-account-number"
elementToXMLBank_account_number :: Xsd.XsdString -> [Content ()]
elementToXMLBank_account_number = schemaTypeToXML "bank-account-number"
 
data Message_value = Message_value
        deriving (Eq,Show)
instance SchemaType Message_value where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Message_value
    schemaTypeToXML s x@Message_value{} =
        toXMLElement s []
            []
 
elementMessage_value :: XMLParser Xsd.XsdString
elementMessage_value = parseSchemaType "message-value"
elementToXMLMessage_value :: Xsd.XsdString -> [Content ()]
elementToXMLMessage_value = schemaTypeToXML "message-value"
 
elementName :: XMLParser Xsd.XsdString
elementName = parseSchemaType "name"
elementToXMLName :: Xsd.XsdString -> [Content ()]
elementToXMLName = schemaTypeToXML "name"
 
data Messages = Messages
        { messages_message :: [Message]
        , messages_three_d_security_info :: Maybe Three_d_security_info
        , messages_plimus_calculated_total :: Maybe Plimus_calculated_total
        }
        deriving (Eq,Show)
instance SchemaType Messages where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Messages
            `apply` many1 (elementMessage)
            `apply` optional (elementThree_d_security_info)
            `apply` optional (elementPlimus_calculated_total)
    schemaTypeToXML s x@Messages{} =
        toXMLElement s []
            [ concatMap (elementToXMLMessage) $ messages_message x
            , maybe [] (elementToXMLThree_d_security_info) $ messages_three_d_security_info x
            , maybe [] (elementToXMLPlimus_calculated_total) $ messages_plimus_calculated_total x
            ]
 
elementMessages :: XMLParser Messages
elementMessages = parseSchemaType "messages"
elementToXMLMessages :: Messages -> [Content ()]
elementToXMLMessages = schemaTypeToXML "messages"
 
data Message = Message
        { message_error_name :: Maybe Xsd.XsdString
        , message_code :: Maybe Xsd.XsdString
        , message_description :: Xsd.XsdString
        , message_invalid_property :: Maybe Invalid_property
        }
        deriving (Eq,Show)
instance SchemaType Message where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Message
            `apply` optional (elementError_name)
            `apply` optional (elementCode)
            `apply` elementDescription
            `apply` optional (elementInvalid_property)
    schemaTypeToXML s x@Message{} =
        toXMLElement s []
            [ maybe [] (elementToXMLError_name) $ message_error_name x
            , maybe [] (elementToXMLCode) $ message_code x
            , elementToXMLDescription $ message_description x
            , maybe [] (elementToXMLInvalid_property) $ message_invalid_property x
            ]
 
elementMessage :: XMLParser Message
elementMessage = parseSchemaType "message"
elementToXMLMessage :: Message -> [Content ()]
elementToXMLMessage = schemaTypeToXML "message"
 
data Three_d_security_info = Three_d_security_info
        { three_d_security_info_payer_authentication_request :: Xsd.XsdString
        , three_d_security_info_acs_link :: Xsd.XsdString
        , three_d_security_info_transaction_id :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Three_d_security_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Three_d_security_info
            `apply` elementPayer_authentication_request
            `apply` elementAcs_link
            `apply` elementTransaction_id
    schemaTypeToXML s x@Three_d_security_info{} =
        toXMLElement s []
            [ elementToXMLPayer_authentication_request $ three_d_security_info_payer_authentication_request x
            , elementToXMLAcs_link $ three_d_security_info_acs_link x
            , elementToXMLTransaction_id $ three_d_security_info_transaction_id x
            ]
 
elementThree_d_security_info :: XMLParser Three_d_security_info
elementThree_d_security_info = parseSchemaType "three-d-security-info"
elementToXMLThree_d_security_info :: Three_d_security_info -> [Content ()]
elementToXMLThree_d_security_info = schemaTypeToXML "three-d-security-info"
 
data Plimus_calculated_total = Plimus_calculated_total
        { plimus_calculated_total_price :: Price
        , plimus_calculated_total_cart :: Maybe Cart
        , plimus_calculated_total_subscription_payment :: Maybe Subscription_payment
        }
        deriving (Eq,Show)
instance SchemaType Plimus_calculated_total where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Plimus_calculated_total
            `apply` elementPrice
            `apply` optional (elementCart)
            `apply` optional (elementSubscription_payment)
    schemaTypeToXML s x@Plimus_calculated_total{} =
        toXMLElement s []
            [ elementToXMLPrice $ plimus_calculated_total_price x
            , maybe [] (elementToXMLCart) $ plimus_calculated_total_cart x
            , maybe [] (elementToXMLSubscription_payment) $ plimus_calculated_total_subscription_payment x
            ]
 
elementPlimus_calculated_total :: XMLParser Plimus_calculated_total
elementPlimus_calculated_total = parseSchemaType "plimus-calculated-total"
elementToXMLPlimus_calculated_total :: Plimus_calculated_total -> [Content ()]
elementToXMLPlimus_calculated_total = schemaTypeToXML "plimus-calculated-total"
 
data Subscription_payment = Subscription_payment
        { subscription_payment_coupons_total :: Maybe Xsd.Decimal
        , subscription_payment_tax :: Maybe Xsd.Decimal
        , subscription_payment_tax_rate :: Maybe Xsd.Decimal
        , subscription_payment_charged_currency :: Xsd.XsdString
        , subscription_payment_total_subscription_charge_cost :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Subscription_payment where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Subscription_payment
            `apply` optional (elementCoupons_total)
            `apply` optional (elementTax)
            `apply` optional (elementTax_rate)
            `apply` elementCharged_currency
            `apply` elementTotal_subscription_charge_cost
    schemaTypeToXML s x@Subscription_payment{} =
        toXMLElement s []
            [ maybe [] (elementToXMLCoupons_total) $ subscription_payment_coupons_total x
            , maybe [] (elementToXMLTax) $ subscription_payment_tax x
            , maybe [] (elementToXMLTax_rate) $ subscription_payment_tax_rate x
            , elementToXMLCharged_currency $ subscription_payment_charged_currency x
            , elementToXMLTotal_subscription_charge_cost $ subscription_payment_total_subscription_charge_cost x
            ]
 
elementSubscription_payment :: XMLParser Subscription_payment
elementSubscription_payment = parseSchemaType "subscription-payment"
elementToXMLSubscription_payment :: Subscription_payment -> [Content ()]
elementToXMLSubscription_payment = schemaTypeToXML "subscription-payment"
 
data Total_subscription_charge_cost = Total_subscription_charge_cost
        deriving (Eq,Show)
instance SchemaType Total_subscription_charge_cost where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Total_subscription_charge_cost
    schemaTypeToXML s x@Total_subscription_charge_cost{} =
        toXMLElement s []
            []
 
elementTotal_subscription_charge_cost :: XMLParser Xsd.Decimal
elementTotal_subscription_charge_cost = parseSchemaType "total-subscription-charge-cost"
elementToXMLTotal_subscription_charge_cost :: Xsd.Decimal -> [Content ()]
elementToXMLTotal_subscription_charge_cost = schemaTypeToXML "total-subscription-charge-cost"
 
data Payer_authentication_request = Payer_authentication_request
        deriving (Eq,Show)
instance SchemaType Payer_authentication_request where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Payer_authentication_request
    schemaTypeToXML s x@Payer_authentication_request{} =
        toXMLElement s []
            []
 
elementPayer_authentication_request :: XMLParser Xsd.XsdString
elementPayer_authentication_request = parseSchemaType "payer-authentication-request"
elementToXMLPayer_authentication_request :: Xsd.XsdString -> [Content ()]
elementToXMLPayer_authentication_request = schemaTypeToXML "payer-authentication-request"
 
data Acs_link = Acs_link
        deriving (Eq,Show)
instance SchemaType Acs_link where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Acs_link
    schemaTypeToXML s x@Acs_link{} =
        toXMLElement s []
            []
 
elementAcs_link :: XMLParser Xsd.XsdString
elementAcs_link = parseSchemaType "acs-link"
elementToXMLAcs_link :: Xsd.XsdString -> [Content ()]
elementToXMLAcs_link = schemaTypeToXML "acs-link"
 
data Invalid_property = Invalid_property
        { invalid_property_name :: Xsd.XsdString
        , invalid_property_message_value :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Invalid_property where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Invalid_property
            `apply` elementName
            `apply` elementMessage_value
    schemaTypeToXML s x@Invalid_property{} =
        toXMLElement s []
            [ elementToXMLName $ invalid_property_name x
            , elementToXMLMessage_value $ invalid_property_message_value x
            ]
 
elementInvalid_property :: XMLParser Invalid_property
elementInvalid_property = parseSchemaType "invalid-property"
elementToXMLInvalid_property :: Invalid_property -> [Content ()]
elementToXMLInvalid_property = schemaTypeToXML "invalid-property"
 
data Error_name = Error_name
        deriving (Eq,Show)
instance SchemaType Error_name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Error_name
    schemaTypeToXML s x@Error_name{} =
        toXMLElement s []
            []
 
elementError_name :: XMLParser Xsd.XsdString
elementError_name = parseSchemaType "error-name"
elementToXMLError_name :: Xsd.XsdString -> [Content ()]
elementToXMLError_name = schemaTypeToXML "error-name"
 
elementDescription :: XMLParser Xsd.XsdString
elementDescription = parseSchemaType "description"
elementToXMLDescription :: Xsd.XsdString -> [Content ()]
elementToXMLDescription = schemaTypeToXML "description"
 
data Code = Code
        deriving (Eq,Show)
instance SchemaType Code where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Code
    schemaTypeToXML s x@Code{} =
        toXMLElement s []
            []
 
elementCode :: XMLParser Xsd.XsdString
elementCode = parseSchemaType "code"
elementToXMLCode :: Xsd.XsdString -> [Content ()]
elementToXMLCode = schemaTypeToXML "code"
 
data Order_history = Order_history
        { order_history_ordering_shopper :: Ordering_shopper
        , order_history_past_orders :: Past_orders
        }
        deriving (Eq,Show)
instance SchemaType Order_history where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Order_history
            `apply` elementOrdering_shopper
            `apply` elementPast_orders
    schemaTypeToXML s x@Order_history{} =
        toXMLElement s []
            [ elementToXMLOrdering_shopper $ order_history_ordering_shopper x
            , elementToXMLPast_orders $ order_history_past_orders x
            ]
 
elementOrder_history :: XMLParser Order_history
elementOrder_history = parseSchemaType "order-history"
elementToXMLOrder_history :: Order_history -> [Content ()]
elementToXMLOrder_history = schemaTypeToXML "order-history"
 
data Past_orders = Past_orders
        { past_orders_url :: [Xsd.XsdString]
        , past_orders_order :: [Order]
        }
        deriving (Eq,Show)
instance SchemaType Past_orders where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Past_orders
            `apply` many (elementUrl)
            `apply` many (elementOrder)
    schemaTypeToXML s x@Past_orders{} =
        toXMLElement s []
            [ concatMap (elementToXMLUrl) $ past_orders_url x
            , concatMap (elementToXMLOrder) $ past_orders_order x
            ]
 
elementPast_orders :: XMLParser Past_orders
elementPast_orders = parseSchemaType "past-orders"
elementToXMLPast_orders :: Past_orders -> [Content ()]
elementToXMLPast_orders = schemaTypeToXML "past-orders"
 
data Param_value = Param_value
        deriving (Eq,Show)
instance SchemaType Param_value where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Param_value
    schemaTypeToXML s x@Param_value{} =
        toXMLElement s []
            []
 
elementParam_value :: XMLParser Xsd.XsdString
elementParam_value = parseSchemaType "param-value"
elementToXMLParam_value :: Xsd.XsdString -> [Content ()]
elementToXMLParam_value = schemaTypeToXML "param-value"
 
data Total_cart_cost = Total_cart_cost
        deriving (Eq,Show)
instance SchemaType Total_cart_cost where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Total_cart_cost
    schemaTypeToXML s x@Total_cart_cost{} =
        toXMLElement s []
            []
 
elementTotal_cart_cost :: XMLParser Xsd.Decimal
elementTotal_cart_cost = parseSchemaType "total-cart-cost"
elementToXMLTotal_cart_cost :: Xsd.Decimal -> [Content ()]
elementToXMLTotal_cart_cost = schemaTypeToXML "total-cart-cost"
 
data Tax = Tax
        deriving (Eq,Show)
instance SchemaType Tax where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Tax
    schemaTypeToXML s x@Tax{} =
        toXMLElement s []
            []
 
elementTax :: XMLParser Xsd.Decimal
elementTax = parseSchemaType "tax"
elementToXMLTax :: Xsd.Decimal -> [Content ()]
elementToXMLTax = schemaTypeToXML "tax"
 
data Tax_recurring = Tax_recurring
        deriving (Eq,Show)
instance SchemaType Tax_recurring where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Tax_recurring
    schemaTypeToXML s x@Tax_recurring{} =
        toXMLElement s []
            []
 
elementTax_recurring :: XMLParser Xsd.Decimal
elementTax_recurring = parseSchemaType "tax-recurring"
elementToXMLTax_recurring :: Xsd.Decimal -> [Content ()]
elementToXMLTax_recurring = schemaTypeToXML "tax-recurring"
 
data Tax_rate = Tax_rate
        deriving (Eq,Show)
instance SchemaType Tax_rate where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Tax_rate
    schemaTypeToXML s x@Tax_rate{} =
        toXMLElement s []
            []
 
elementTax_rate :: XMLParser Xsd.Decimal
elementTax_rate = parseSchemaType "tax-rate"
elementToXMLTax_rate :: Xsd.Decimal -> [Content ()]
elementToXMLTax_rate = schemaTypeToXML "tax-rate"
 
data Soft_descriptor = Soft_descriptor
        deriving (Eq,Show)
instance SchemaType Soft_descriptor where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Soft_descriptor
    schemaTypeToXML s x@Soft_descriptor{} =
        toXMLElement s []
            []
 
elementSoft_descriptor :: XMLParser Xsd.XsdString
elementSoft_descriptor = parseSchemaType "soft-descriptor"
elementToXMLSoft_descriptor :: Xsd.XsdString -> [Content ()]
elementToXMLSoft_descriptor = schemaTypeToXML "soft-descriptor"
 
data Sku_parameter = Sku_parameter
        { sku_parameter_param_name :: Xsd.XsdString
        , sku_parameter_param_value :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Sku_parameter where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_parameter
            `apply` elementParam_name
            `apply` elementParam_value
    schemaTypeToXML s x@Sku_parameter{} =
        toXMLElement s []
            [ elementToXMLParam_name $ sku_parameter_param_name x
            , elementToXMLParam_value $ sku_parameter_param_value x
            ]
 
elementSku_parameter :: XMLParser Sku_parameter
elementSku_parameter = parseSchemaType "sku-parameter"
elementToXMLSku_parameter :: Sku_parameter -> [Content ()]
elementToXMLSku_parameter = schemaTypeToXML "sku-parameter"
 
data Shopper_id = Shopper_id
        deriving (Eq,Show)
instance SchemaType Shopper_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Shopper_id
    schemaTypeToXML s x@Shopper_id{} =
        toXMLElement s []
            []
 
elementShopper_id :: XMLParser Xs.Long
elementShopper_id = parseSchemaType "shopper-id"
elementToXMLShopper_id :: Xs.Long -> [Content ()]
elementToXMLShopper_id = schemaTypeToXML "shopper-id"
 
data Ordering_shopper = Ordering_shopper
        { ordering_shopper_shopper_id :: Maybe Xs.Long
        , ordering_shopper_seller_shopper_id :: Maybe Xsd.XsdString
        , ordering_shopper_credit_card :: Maybe Credit_card
        , ordering_shopper_ecp :: Maybe Ecp
        , ordering_shopper_invoice_contact_info :: Maybe Invoice_contact_info
        , ordering_shopper_paypal :: Maybe Paypal
        , ordering_shopper_wire :: Maybe Wire
        , ordering_shopper_local_bank_transfer :: Maybe Local_bank_transfer
        , ordering_shopper_web_info :: Maybe Web_info
        , ordering_shopper_three_d_authenticated_info :: Maybe Three_d_authenticated_info
        }
        deriving (Eq,Show)
instance SchemaType Ordering_shopper where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Ordering_shopper
            `apply` optional (elementShopper_id)
            `apply` optional (elementSeller_shopper_id)
            `apply` optional (elementCredit_card)
            `apply` optional (elementEcp)
            `apply` optional (elementInvoice_contact_info)
            `apply` optional (elementPaypal)
            `apply` optional (elementWire)
            `apply` optional (elementLocal_bank_transfer)
            `apply` optional (elementWeb_info)
            `apply` optional (elementThree_d_authenticated_info)
    schemaTypeToXML s x@Ordering_shopper{} =
        toXMLElement s []
            [ maybe [] (elementToXMLShopper_id) $ ordering_shopper_shopper_id x
            , maybe [] (elementToXMLSeller_shopper_id) $ ordering_shopper_seller_shopper_id x
            , maybe [] (elementToXMLCredit_card) $ ordering_shopper_credit_card x
            , maybe [] (elementToXMLEcp) $ ordering_shopper_ecp x
            , maybe [] (elementToXMLInvoice_contact_info) $ ordering_shopper_invoice_contact_info x
            , maybe [] (elementToXMLPaypal) $ ordering_shopper_paypal x
            , maybe [] (elementToXMLWire) $ ordering_shopper_wire x
            , maybe [] (elementToXMLLocal_bank_transfer) $ ordering_shopper_local_bank_transfer x
            , maybe [] (elementToXMLWeb_info) $ ordering_shopper_web_info x
            , maybe [] (elementToXMLThree_d_authenticated_info) $ ordering_shopper_three_d_authenticated_info x
            ]
 
elementOrdering_shopper :: XMLParser Ordering_shopper
elementOrdering_shopper = parseSchemaType "ordering-shopper"
elementToXMLOrdering_shopper :: Ordering_shopper -> [Content ()]
elementToXMLOrdering_shopper = schemaTypeToXML "ordering-shopper"
 
data Local_bank_transfer = Local_bank_transfer
        { local_bank_transfer_bank_country :: [Xsd.XsdString]
        }
        deriving (Eq,Show)
instance SchemaType Local_bank_transfer where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Local_bank_transfer
            `apply` between (Occurs (Just 1) Nothing)
                            (elementBank_country)
    schemaTypeToXML s x@Local_bank_transfer{} =
        toXMLElement s []
            [ concatMap (elementToXMLBank_country) $ local_bank_transfer_bank_country x
            ]
 
elementLocal_bank_transfer :: XMLParser Local_bank_transfer
elementLocal_bank_transfer = parseSchemaType "local-bank-transfer"
elementToXMLLocal_bank_transfer :: Local_bank_transfer -> [Content ()]
elementToXMLLocal_bank_transfer = schemaTypeToXML "local-bank-transfer"
 
data Post_sale_info = Post_sale_info
        { post_sale_info_invoice_id :: Maybe Xs.Long
        , post_sale_info_invoice_status :: Maybe Xsd.XsdString
        , post_sale_info_soft_descriptor :: Maybe Xsd.XsdString
        , post_sale_info_invoices :: Invoices
        }
        deriving (Eq,Show)
instance SchemaType Post_sale_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Post_sale_info
            `apply` optional (elementInvoice_id)
            `apply` optional (elementInvoice_status)
            `apply` optional (elementSoft_descriptor)
            `apply` elementInvoices
    schemaTypeToXML s x@Post_sale_info{} =
        toXMLElement s []
            [ maybe [] (elementToXMLInvoice_id) $ post_sale_info_invoice_id x
            , maybe [] (elementToXMLInvoice_status) $ post_sale_info_invoice_status x
            , maybe [] (elementToXMLSoft_descriptor) $ post_sale_info_soft_descriptor x
            , elementToXMLInvoices $ post_sale_info_invoices x
            ]
 
elementPost_sale_info :: XMLParser Post_sale_info
elementPost_sale_info = parseSchemaType "post-sale-info"
elementToXMLPost_sale_info :: Post_sale_info -> [Content ()]
elementToXMLPost_sale_info = schemaTypeToXML "post-sale-info"
 
data Invoices = Invoices
        { invoices_invoice :: [Invoice]
        }
        deriving (Eq,Show)
instance SchemaType Invoices where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Invoices
            `apply` many1 (elementInvoice)
    schemaTypeToXML s x@Invoices{} =
        toXMLElement s []
            [ concatMap (elementToXMLInvoice) $ invoices_invoice x
            ]
 
elementInvoices :: XMLParser Invoices
elementInvoices = parseSchemaType "invoices"
elementToXMLInvoices :: Invoices -> [Content ()]
elementToXMLInvoices = schemaTypeToXML "invoices"
 
data Invoice = Invoice
        { invoice_invoice_id :: Xs.Long
        , invoice_url :: Xsd.XsdString
        , invoice_financial_transactions :: Financial_transactions
        }
        deriving (Eq,Show)
instance SchemaType Invoice where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Invoice
            `apply` elementInvoice_id
            `apply` elementUrl
            `apply` elementFinancial_transactions
    schemaTypeToXML s x@Invoice{} =
        toXMLElement s []
            [ elementToXMLInvoice_id $ invoice_invoice_id x
            , elementToXMLUrl $ invoice_url x
            , elementToXMLFinancial_transactions $ invoice_financial_transactions x
            ]
 
elementInvoice :: XMLParser Invoice
elementInvoice = parseSchemaType "invoice"
elementToXMLInvoice :: Invoice -> [Content ()]
elementToXMLInvoice = schemaTypeToXML "invoice"
 
data Financial_transactions = Financial_transactions
        { financial_transactions_financial_transaction :: [Financial_transaction]
        }
        deriving (Eq,Show)
instance SchemaType Financial_transactions where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Financial_transactions
            `apply` many1 (elementFinancial_transaction)
    schemaTypeToXML s x@Financial_transactions{} =
        toXMLElement s []
            [ concatMap (elementToXMLFinancial_transaction) $ financial_transactions_financial_transaction x
            ]
 
elementFinancial_transactions :: XMLParser Financial_transactions
elementFinancial_transactions = parseSchemaType "financial-transactions"
elementToXMLFinancial_transactions :: Financial_transactions -> [Content ()]
elementToXMLFinancial_transactions = schemaTypeToXML "financial-transactions"
 
data Financial_transaction = Financial_transaction
        { financial_transaction_status :: Xsd.XsdString
        , financial_transaction_date_due :: Xsd.XsdString
        , financial_transaction_date_created :: Xsd.XsdString
        , financial_transaction_amount :: Xsd.Decimal
        , financial_transaction_tax :: Maybe Xsd.Decimal
        , financial_transaction_tax_rate :: Maybe Xsd.Decimal
        , financial_transaction_currency :: Xsd.XsdString
        , financial_transaction_soft_descriptor :: Xsd.XsdString
        , financial_transaction_payment_method :: Xsd.XsdString
        , financial_transaction_target_balance :: Xsd.XsdString
        , financial_transaction_credit_card :: Credit_card
        , financial_transaction_paypal_transaction_data :: Maybe Paypal_transaction_data
        , financial_transaction_ecp :: Maybe Ecp
        , financial_transaction_invoice_contact_info :: Invoice_contact_info
        , financial_transaction_skus :: Skus
        }
        deriving (Eq,Show)
instance SchemaType Financial_transaction where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Financial_transaction
            `apply` elementStatus
            `apply` elementDate_due
            `apply` elementDate_created
            `apply` elementAmount
            `apply` optional (elementTax)
            `apply` optional (elementTax_rate)
            `apply` elementCurrency
            `apply` elementSoft_descriptor
            `apply` elementPayment_method
            `apply` elementTarget_balance
            `apply` elementCredit_card
            `apply` optional (elementPaypal_transaction_data)
            `apply` optional (elementEcp)
            `apply` elementInvoice_contact_info
            `apply` elementSkus
    schemaTypeToXML s x@Financial_transaction{} =
        toXMLElement s []
            [ elementToXMLStatus $ financial_transaction_status x
            , elementToXMLDate_due $ financial_transaction_date_due x
            , elementToXMLDate_created $ financial_transaction_date_created x
            , elementToXMLAmount $ financial_transaction_amount x
            , maybe [] (elementToXMLTax) $ financial_transaction_tax x
            , maybe [] (elementToXMLTax_rate) $ financial_transaction_tax_rate x
            , elementToXMLCurrency $ financial_transaction_currency x
            , elementToXMLSoft_descriptor $ financial_transaction_soft_descriptor x
            , elementToXMLPayment_method $ financial_transaction_payment_method x
            , elementToXMLTarget_balance $ financial_transaction_target_balance x
            , elementToXMLCredit_card $ financial_transaction_credit_card x
            , maybe [] (elementToXMLPaypal_transaction_data) $ financial_transaction_paypal_transaction_data x
            , maybe [] (elementToXMLEcp) $ financial_transaction_ecp x
            , elementToXMLInvoice_contact_info $ financial_transaction_invoice_contact_info x
            , elementToXMLSkus $ financial_transaction_skus x
            ]
 
elementFinancial_transaction :: XMLParser Financial_transaction
elementFinancial_transaction = parseSchemaType "financial-transaction"
elementToXMLFinancial_transaction :: Financial_transaction -> [Content ()]
elementToXMLFinancial_transaction = schemaTypeToXML "financial-transaction"
 
data Skus = Skus
        { skus_sku :: [Sku]
        }
        deriving (Eq,Show)
instance SchemaType Skus where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Skus
            `apply` many1 (elementSku)
    schemaTypeToXML s x@Skus{} =
        toXMLElement s []
            [ concatMap (elementToXMLSku) $ skus_sku x
            ]
 
elementSkus :: XMLParser Skus
elementSkus = parseSchemaType "skus"
elementToXMLSkus :: Skus -> [Content ()]
elementToXMLSkus = schemaTypeToXML "skus"
 
data Status = Status
        deriving (Eq,Show)
instance SchemaType Status where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Status
    schemaTypeToXML s x@Status{} =
        toXMLElement s []
            []
 
elementStatus :: XMLParser Xsd.XsdString
elementStatus = parseSchemaType "status"
elementToXMLStatus :: Xsd.XsdString -> [Content ()]
elementToXMLStatus = schemaTypeToXML "status"
 
data Date_due = Date_due
        deriving (Eq,Show)
instance SchemaType Date_due where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Date_due
    schemaTypeToXML s x@Date_due{} =
        toXMLElement s []
            []
 
elementDate_due :: XMLParser Xsd.XsdString
elementDate_due = parseSchemaType "date-due"
elementToXMLDate_due :: Xsd.XsdString -> [Content ()]
elementToXMLDate_due = schemaTypeToXML "date-due"
 
data Date_created = Date_created
        deriving (Eq,Show)
instance SchemaType Date_created where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Date_created
    schemaTypeToXML s x@Date_created{} =
        toXMLElement s []
            []
 
elementDate_created :: XMLParser Xsd.XsdString
elementDate_created = parseSchemaType "date-created"
elementToXMLDate_created :: Xsd.XsdString -> [Content ()]
elementToXMLDate_created = schemaTypeToXML "date-created"
 
data Payment_method = Payment_method
        deriving (Eq,Show)
instance SchemaType Payment_method where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Payment_method
    schemaTypeToXML s x@Payment_method{} =
        toXMLElement s []
            []
 
elementPayment_method :: XMLParser Xsd.XsdString
elementPayment_method = parseSchemaType "payment-method"
elementToXMLPayment_method :: Xsd.XsdString -> [Content ()]
elementToXMLPayment_method = schemaTypeToXML "payment-method"
 
data Invoice_status = Invoice_status
        deriving (Eq,Show)
instance SchemaType Invoice_status where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Invoice_status
    schemaTypeToXML s x@Invoice_status{} =
        toXMLElement s []
            []
 
elementInvoice_status :: XMLParser Xsd.XsdString
elementInvoice_status = parseSchemaType "invoice-status"
elementToXMLInvoice_status :: Xsd.XsdString -> [Content ()]
elementToXMLInvoice_status = schemaTypeToXML "invoice-status"
 
data Order = Order
        { order_order_id :: Xs.Long
        , order_soft_descriptor :: Maybe Xsd.XsdString
        , order_ordering_shopper :: Ordering_shopper
        , order_transaction_payment_info :: Maybe Transaction_payment_info
        , order_cart :: Cart
        , order_expected_total_price :: Maybe Expected_total_price
        , order_affiliate_id :: [Xs.Long]
        , order_post_sale_info :: Maybe Post_sale_info
        , order_fulfillment :: [Fulfillment]
        }
        deriving (Eq,Show)
instance SchemaType Order where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Order
            `apply` elementOrder_id
            `apply` optional (elementSoft_descriptor)
            `apply` elementOrdering_shopper
            `apply` optional (elementTransaction_payment_info)
            `apply` elementCart
            `apply` optional (elementExpected_total_price)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementAffiliate_id)
            `apply` optional (elementPost_sale_info)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementFulfillment)
    schemaTypeToXML s x@Order{} =
        toXMLElement s []
            [ elementToXMLOrder_id $ order_order_id x
            , maybe [] (elementToXMLSoft_descriptor) $ order_soft_descriptor x
            , elementToXMLOrdering_shopper $ order_ordering_shopper x
            , maybe [] (elementToXMLTransaction_payment_info) $ order_transaction_payment_info x
            , elementToXMLCart $ order_cart x
            , maybe [] (elementToXMLExpected_total_price) $ order_expected_total_price x
            , concatMap (elementToXMLAffiliate_id) $ order_affiliate_id x
            , maybe [] (elementToXMLPost_sale_info) $ order_post_sale_info x
            , concatMap (elementToXMLFulfillment) $ order_fulfillment x
            ]
 
elementOrder :: XMLParser Order
elementOrder = parseSchemaType "order"
elementToXMLOrder :: Order -> [Content ()]
elementToXMLOrder = schemaTypeToXML "order"
 
data Param_name = Param_name
        deriving (Eq,Show)
instance SchemaType Param_name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Param_name
    schemaTypeToXML s x@Param_name{} =
        toXMLElement s []
            []
 
elementParam_name :: XMLParser Xsd.XsdString
elementParam_name = parseSchemaType "param-name"
elementToXMLParam_name :: Xsd.XsdString -> [Content ()]
elementToXMLParam_name = schemaTypeToXML "param-name"
 
data Item_sub_total = Item_sub_total
        deriving (Eq,Show)
instance SchemaType Item_sub_total where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Item_sub_total
    schemaTypeToXML s x@Item_sub_total{} =
        toXMLElement s []
            []
 
elementItem_sub_total :: XMLParser Xsd.Decimal
elementItem_sub_total = parseSchemaType "item-sub-total"
elementToXMLItem_sub_total :: Xsd.Decimal -> [Content ()]
elementToXMLItem_sub_total = schemaTypeToXML "item-sub-total"
 
data Item_coupon_total = Item_coupon_total
        deriving (Eq,Show)
instance SchemaType Item_coupon_total where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Item_coupon_total
    schemaTypeToXML s x@Item_coupon_total{} =
        toXMLElement s []
            []
 
elementItem_coupon_total :: XMLParser Xsd.Decimal
elementItem_coupon_total = parseSchemaType "item-coupon-total"
elementToXMLItem_coupon_total :: Xsd.Decimal -> [Content ()]
elementToXMLItem_coupon_total = schemaTypeToXML "item-coupon-total"
 
data Item_total = Item_total
        deriving (Eq,Show)
instance SchemaType Item_total where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Item_total
    schemaTypeToXML s x@Item_total{} =
        toXMLElement s []
            []
 
elementItem_total :: XMLParser Xsd.Decimal
elementItem_total = parseSchemaType "item-total"
elementToXMLItem_total :: Xsd.Decimal -> [Content ()]
elementToXMLItem_total = schemaTypeToXML "item-total"
 
data Invoice_id = Invoice_id
        deriving (Eq,Show)
instance SchemaType Invoice_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Invoice_id
    schemaTypeToXML s x@Invoice_id{} =
        toXMLElement s []
            []
 
elementInvoice_id :: XMLParser Xs.Long
elementInvoice_id = parseSchemaType "invoice-id"
elementToXMLInvoice_id :: Xs.Long -> [Content ()]
elementToXMLInvoice_id = schemaTypeToXML "invoice-id"
 
data Order_id = Order_id
        deriving (Eq,Show)
instance SchemaType Order_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Order_id
    schemaTypeToXML s x@Order_id{} =
        toXMLElement s []
            []
 
elementOrder_id :: XMLParser Xs.Long
elementOrder_id = parseSchemaType "order-id"
elementToXMLOrder_id :: Xs.Long -> [Content ()]
elementToXMLOrder_id = schemaTypeToXML "order-id"
 
data Affiliate_id = Affiliate_id
        deriving (Eq,Show)
instance SchemaType Affiliate_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Affiliate_id
    schemaTypeToXML s x@Affiliate_id{} =
        toXMLElement s []
            []
 
elementAffiliate_id :: XMLParser Xs.Long
elementAffiliate_id = parseSchemaType "affiliate-id"
elementToXMLAffiliate_id :: Xs.Long -> [Content ()]
elementToXMLAffiliate_id = schemaTypeToXML "affiliate-id"
 
data Expected_total_price = Expected_total_price
        { expected_total_price_amount :: Xsd.Decimal
        , expected_total_price_currency :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Expected_total_price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Expected_total_price
            `apply` elementAmount
            `apply` elementCurrency
    schemaTypeToXML s x@Expected_total_price{} =
        toXMLElement s []
            [ elementToXMLAmount $ expected_total_price_amount x
            , elementToXMLCurrency $ expected_total_price_currency x
            ]
 
elementExpected_total_price :: XMLParser Expected_total_price
elementExpected_total_price = parseSchemaType "expected-total-price"
elementToXMLExpected_total_price :: Expected_total_price -> [Content ()]
elementToXMLExpected_total_price = schemaTypeToXML "expected-total-price"
 
data Transaction_payment_info = Transaction_payment_info
        { transaction_payment_info_paypal_info :: Maybe Paypal_info
        , transaction_payment_info_local_bank_transfer_info :: Maybe Local_bank_transfer_info
        }
        deriving (Eq,Show)
instance SchemaType Transaction_payment_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Transaction_payment_info
            `apply` optional (elementPaypal_info)
            `apply` optional (elementLocal_bank_transfer_info)
    schemaTypeToXML s x@Transaction_payment_info{} =
        toXMLElement s []
            [ maybe [] (elementToXMLPaypal_info) $ transaction_payment_info_paypal_info x
            , maybe [] (elementToXMLLocal_bank_transfer_info) $ transaction_payment_info_local_bank_transfer_info x
            ]
 
elementTransaction_payment_info :: XMLParser Transaction_payment_info
elementTransaction_payment_info = parseSchemaType "transaction-payment-info"
elementToXMLTransaction_payment_info :: Transaction_payment_info -> [Content ()]
elementToXMLTransaction_payment_info = schemaTypeToXML "transaction-payment-info"
 
data Coupons = Coupons
        { coupons_coupon :: [Xsd.XsdString]
        , coupons_coupons_total :: Maybe Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Coupons where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupons
            `apply` many1 (elementCoupon)
            `apply` optional (elementCoupons_total)
    schemaTypeToXML s x@Coupons{} =
        toXMLElement s []
            [ concatMap (elementToXMLCoupon) $ coupons_coupon x
            , maybe [] (elementToXMLCoupons_total) $ coupons_coupons_total x
            ]
 
elementCoupons :: XMLParser Coupons
elementCoupons = parseSchemaType "coupons"
elementToXMLCoupons :: Coupons -> [Content ()]
elementToXMLCoupons = schemaTypeToXML "coupons"
 
data Coupon = Coupon
        deriving (Eq,Show)
instance SchemaType Coupon where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupon
    schemaTypeToXML s x@Coupon{} =
        toXMLElement s []
            []
 
elementCoupon :: XMLParser Xsd.XsdString
elementCoupon = parseSchemaType "coupon"
elementToXMLCoupon :: Xsd.XsdString -> [Content ()]
elementToXMLCoupon = schemaTypeToXML "coupon"
 
elementCdod :: XMLParser Xsd.Boolean
elementCdod = parseSchemaType "cdod"
elementToXMLCdod :: Xsd.Boolean -> [Content ()]
elementToXMLCdod = schemaTypeToXML "cdod"
 
data Edw_duration = Edw_duration
        deriving (Eq,Show)
instance SchemaType Edw_duration where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Edw_duration
    schemaTypeToXML s x@Edw_duration{} =
        toXMLElement s []
            []
 
elementEdw_duration :: XMLParser Xs.Int
elementEdw_duration = parseSchemaType "edw-duration"
elementToXMLEdw_duration :: Xs.Int -> [Content ()]
elementToXMLEdw_duration = schemaTypeToXML "edw-duration"
 
data Charged_currency = Charged_currency
        deriving (Eq,Show)
instance SchemaType Charged_currency where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Charged_currency
    schemaTypeToXML s x@Charged_currency{} =
        toXMLElement s []
            []
 
elementCharged_currency :: XMLParser Xsd.XsdString
elementCharged_currency = parseSchemaType "charged-currency"
elementToXMLCharged_currency :: Xsd.XsdString -> [Content ()]
elementToXMLCharged_currency = schemaTypeToXML "charged-currency"
 
data Cart_item = Cart_item
        { cart_item_sku :: Sku
        , cart_item_quantity :: Xs.Int
        , cart_item_sku_parameter :: [Sku_parameter]
        , cart_item_url :: Maybe Xsd.XsdString
        , cart_item_item_sub_total :: Maybe Xsd.Decimal
        , cart_item_item_coupon_total :: Maybe Xsd.Decimal
        , cart_item_item_total :: Maybe Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Cart_item where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Cart_item
            `apply` elementSku
            `apply` elementQuantity
            `apply` many (elementSku_parameter)
            `apply` optional (elementUrl)
            `apply` optional (elementItem_sub_total)
            `apply` optional (elementItem_coupon_total)
            `apply` optional (elementItem_total)
    schemaTypeToXML s x@Cart_item{} =
        toXMLElement s []
            [ elementToXMLSku $ cart_item_sku x
            , elementToXMLQuantity $ cart_item_quantity x
            , concatMap (elementToXMLSku_parameter) $ cart_item_sku_parameter x
            , maybe [] (elementToXMLUrl) $ cart_item_url x
            , maybe [] (elementToXMLItem_sub_total) $ cart_item_item_sub_total x
            , maybe [] (elementToXMLItem_coupon_total) $ cart_item_item_coupon_total x
            , maybe [] (elementToXMLItem_total) $ cart_item_item_total x
            ]
 
elementCart_item :: XMLParser Cart_item
elementCart_item = parseSchemaType "cart-item"
elementToXMLCart_item :: Cart_item -> [Content ()]
elementToXMLCart_item = schemaTypeToXML "cart-item"
 
data Cart = Cart
        { cart_charged_currency :: Maybe Xsd.XsdString
        , cart_cart_item :: [Cart_item]
        , cart_coupons :: Maybe Coupons
        , cart_edw_duration :: Maybe Xs.Int
        , cart_cdod :: Maybe Xsd.Boolean
        , cart_tax :: Maybe Xsd.Decimal
        , cart_tax_rate :: Maybe Xsd.Decimal
        , cart_total_cart_cost :: Maybe Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Cart where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Cart
            `apply` optional (elementCharged_currency)
            `apply` many1 (elementCart_item)
            `apply` optional (elementCoupons)
            `apply` optional (elementEdw_duration)
            `apply` optional (elementCdod)
            `apply` optional (elementTax)
            `apply` optional (elementTax_rate)
            `apply` optional (elementTotal_cart_cost)
    schemaTypeToXML s x@Cart{} =
        toXMLElement s []
            [ maybe [] (elementToXMLCharged_currency) $ cart_charged_currency x
            , concatMap (elementToXMLCart_item) $ cart_cart_item x
            , maybe [] (elementToXMLCoupons) $ cart_coupons x
            , maybe [] (elementToXMLEdw_duration) $ cart_edw_duration x
            , maybe [] (elementToXMLCdod) $ cart_cdod x
            , maybe [] (elementToXMLTax) $ cart_tax x
            , maybe [] (elementToXMLTax_rate) $ cart_tax_rate x
            , maybe [] (elementToXMLTotal_cart_cost) $ cart_total_cart_cost x
            ]
 
elementCart :: XMLParser Cart
elementCart = parseSchemaType "cart"
elementToXMLCart :: Cart -> [Content ()]
elementToXMLCart = schemaTypeToXML "cart"
 
data Coupons_total = Coupons_total
        deriving (Eq,Show)
instance SchemaType Coupons_total where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Coupons_total
    schemaTypeToXML s x@Coupons_total{} =
        toXMLElement s []
            []
 
elementCoupons_total :: XMLParser Xsd.Decimal
elementCoupons_total = parseSchemaType "coupons-total"
elementToXMLCoupons_total :: Xsd.Decimal -> [Content ()]
elementToXMLCoupons_total = schemaTypeToXML "coupons-total"
 
data Url = Url
        deriving (Eq,Show)
instance SchemaType Url where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Url
    schemaTypeToXML s x@Url{} =
        toXMLElement s []
            []
 
elementUrl :: XMLParser Xsd.XsdString
elementUrl = parseSchemaType "url"
elementToXMLUrl :: Xsd.XsdString -> [Content ()]
elementToXMLUrl = schemaTypeToXML "url"
 
data Target_balance = Target_balance
        deriving (Eq,Show)
instance SchemaType Target_balance where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Target_balance
    schemaTypeToXML s x@Target_balance{} =
        toXMLElement s []
            []
 
elementTarget_balance :: XMLParser Xsd.XsdString
elementTarget_balance = parseSchemaType "target-balance"
elementToXMLTarget_balance :: Xsd.XsdString -> [Content ()]
elementToXMLTarget_balance = schemaTypeToXML "target-balance"
 
data Bank_country = Bank_country
        deriving (Eq,Show)
instance SchemaType Bank_country where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Bank_country
    schemaTypeToXML s x@Bank_country{} =
        toXMLElement s []
            []
 
elementBank_country :: XMLParser Xsd.XsdString
elementBank_country = parseSchemaType "bank-country"
elementToXMLBank_country :: Xsd.XsdString -> [Content ()]
elementToXMLBank_country = schemaTypeToXML "bank-country"
 
data Wire = Wire
        deriving (Eq,Show)
instance SchemaType Wire where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Wire
    schemaTypeToXML s x@Wire{} =
        toXMLElement s []
            []
 
elementWire :: XMLParser Wire
elementWire = parseSchemaType "wire"
elementToXMLWire :: Wire -> [Content ()]
elementToXMLWire = schemaTypeToXML "wire"
 
data Seller_shopper_id = Seller_shopper_id
        deriving (Eq,Show)
instance SchemaType Seller_shopper_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Seller_shopper_id
    schemaTypeToXML s x@Seller_shopper_id{} =
        toXMLElement s []
            []
 
elementSeller_shopper_id :: XMLParser Xsd.XsdString
elementSeller_shopper_id = parseSchemaType "seller-shopper-id"
elementToXMLSeller_shopper_id :: Xsd.XsdString -> [Content ()]
elementToXMLSeller_shopper_id = schemaTypeToXML "seller-shopper-id"
 
data Param_decryption = Param_decryption
        { param_decryption_encrypted_token :: Maybe Xsd.XsdString
        , param_decryption_decrypted_token :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Param_decryption where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Param_decryption
            `apply` optional (elementEncrypted_token)
            `apply` optional (elementDecrypted_token)
    schemaTypeToXML s x@Param_decryption{} =
        toXMLElement s []
            [ maybe [] (elementToXMLEncrypted_token) $ param_decryption_encrypted_token x
            , maybe [] (elementToXMLDecrypted_token) $ param_decryption_decrypted_token x
            ]
 
elementParam_decryption :: XMLParser Param_decryption
elementParam_decryption = parseSchemaType "param-decryption"
elementToXMLParam_decryption :: Param_decryption -> [Content ()]
elementToXMLParam_decryption = schemaTypeToXML "param-decryption"
 
data Decrypted_token = Decrypted_token
        deriving (Eq,Show)
instance SchemaType Decrypted_token where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Decrypted_token
    schemaTypeToXML s x@Decrypted_token{} =
        toXMLElement s []
            []
 
elementDecrypted_token :: XMLParser Xsd.XsdString
elementDecrypted_token = parseSchemaType "decrypted-token"
elementToXMLDecrypted_token :: Xsd.XsdString -> [Content ()]
elementToXMLDecrypted_token = schemaTypeToXML "decrypted-token"
 
data Param_encryption = Param_encryption
        { param_encryption_encrypted_token :: Maybe Xsd.XsdString
        , param_encryption_parameters :: Maybe Parameters
        }
        deriving (Eq,Show)
instance SchemaType Param_encryption where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Param_encryption
            `apply` optional (elementEncrypted_token)
            `apply` optional (elementParameters)
    schemaTypeToXML s x@Param_encryption{} =
        toXMLElement s []
            [ maybe [] (elementToXMLEncrypted_token) $ param_encryption_encrypted_token x
            , maybe [] (elementToXMLParameters) $ param_encryption_parameters x
            ]
 
elementParam_encryption :: XMLParser Param_encryption
elementParam_encryption = parseSchemaType "param-encryption"
elementToXMLParam_encryption :: Param_encryption -> [Content ()]
elementToXMLParam_encryption = schemaTypeToXML "param-encryption"
 
data Parameters = Parameters
        { parameters_parameter :: [Parameter]
        }
        deriving (Eq,Show)
instance SchemaType Parameters where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Parameters
            `apply` many (elementParameter)
    schemaTypeToXML s x@Parameters{} =
        toXMLElement s []
            [ concatMap (elementToXMLParameter) $ parameters_parameter x
            ]
 
elementParameters :: XMLParser Parameters
elementParameters = parseSchemaType "parameters"
elementToXMLParameters :: Parameters -> [Content ()]
elementToXMLParameters = schemaTypeToXML "parameters"
 
data Parameter = Parameter
        { parameter_param_key :: Xsd.XsdString
        , parameter_param_value :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Parameter where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Parameter
            `apply` elementParam_key
            `apply` elementParam_value
    schemaTypeToXML s x@Parameter{} =
        toXMLElement s []
            [ elementToXMLParam_key $ parameter_param_key x
            , elementToXMLParam_value $ parameter_param_value x
            ]
 
elementParameter :: XMLParser Parameter
elementParameter = parseSchemaType "parameter"
elementToXMLParameter :: Parameter -> [Content ()]
elementToXMLParameter = schemaTypeToXML "parameter"
 
data Encrypted_token = Encrypted_token
        deriving (Eq,Show)
instance SchemaType Encrypted_token where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Encrypted_token
    schemaTypeToXML s x@Encrypted_token{} =
        toXMLElement s []
            []
 
elementEncrypted_token :: XMLParser Xsd.XsdString
elementEncrypted_token = parseSchemaType "encrypted-token"
elementToXMLEncrypted_token :: Xsd.XsdString -> [Content ()]
elementToXMLEncrypted_token = schemaTypeToXML "encrypted-token"
 
data Param_key = Param_key
        deriving (Eq,Show)
instance SchemaType Param_key where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Param_key
    schemaTypeToXML s x@Param_key{} =
        toXMLElement s []
            []
 
elementParam_key :: XMLParser Xsd.XsdString
elementParam_key = parseSchemaType "param-key"
elementToXMLParam_key :: Xsd.XsdString -> [Content ()]
elementToXMLParam_key = schemaTypeToXML "param-key"
 
data Paypal_info = Paypal_info
        { paypal_info_url :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Paypal_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Paypal_info
            `apply` elementUrl
    schemaTypeToXML s x@Paypal_info{} =
        toXMLElement s []
            [ elementToXMLUrl $ paypal_info_url x
            ]
 
elementPaypal_info :: XMLParser Paypal_info
elementPaypal_info = parseSchemaType "paypal-info"
elementToXMLPaypal_info :: Paypal_info -> [Content ()]
elementToXMLPaypal_info = schemaTypeToXML "paypal-info"
 
data Paypal = Paypal
        deriving (Eq,Show)
instance SchemaType Paypal where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Paypal
    schemaTypeToXML s x@Paypal{} =
        toXMLElement s []
            []
 
elementPaypal :: XMLParser Paypal
elementPaypal = parseSchemaType "paypal"
elementToXMLPaypal :: Paypal -> [Content ()]
elementToXMLPaypal = schemaTypeToXML "paypal"
 
data Paypal_transaction_data = Paypal_transaction_data
        { paypal_transaction_data_pp_transaction_id :: Maybe Xsd.XsdString
        , paypal_transaction_data_pp_subscription_id :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Paypal_transaction_data where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Paypal_transaction_data
            `apply` optional (elementPp_transaction_id)
            `apply` optional (elementPp_subscription_id)
    schemaTypeToXML s x@Paypal_transaction_data{} =
        toXMLElement s []
            [ maybe [] (elementToXMLPp_transaction_id) $ paypal_transaction_data_pp_transaction_id x
            , maybe [] (elementToXMLPp_subscription_id) $ paypal_transaction_data_pp_subscription_id x
            ]
 
elementPaypal_transaction_data :: XMLParser Paypal_transaction_data
elementPaypal_transaction_data = parseSchemaType "paypal-transaction-data"
elementToXMLPaypal_transaction_data :: Paypal_transaction_data -> [Content ()]
elementToXMLPaypal_transaction_data = schemaTypeToXML "paypal-transaction-data"
 
data Pp_transaction_id = Pp_transaction_id
        deriving (Eq,Show)
instance SchemaType Pp_transaction_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Pp_transaction_id
    schemaTypeToXML s x@Pp_transaction_id{} =
        toXMLElement s []
            []
 
elementPp_transaction_id :: XMLParser Xsd.XsdString
elementPp_transaction_id = parseSchemaType "pp-transaction-id"
elementToXMLPp_transaction_id :: Xsd.XsdString -> [Content ()]
elementToXMLPp_transaction_id = schemaTypeToXML "pp-transaction-id"
 
data Paypal_subscription = Paypal_subscription
        { paypal_subscription_pp_subscription_id :: Xsd.XsdString
        , paypal_subscription_pp_original_transaction_id :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Paypal_subscription where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Paypal_subscription
            `apply` elementPp_subscription_id
            `apply` elementPp_original_transaction_id
    schemaTypeToXML s x@Paypal_subscription{} =
        toXMLElement s []
            [ elementToXMLPp_subscription_id $ paypal_subscription_pp_subscription_id x
            , elementToXMLPp_original_transaction_id $ paypal_subscription_pp_original_transaction_id x
            ]
 
elementPaypal_subscription :: XMLParser Paypal_subscription
elementPaypal_subscription = parseSchemaType "paypal-subscription"
elementToXMLPaypal_subscription :: Paypal_subscription -> [Content ()]
elementToXMLPaypal_subscription = schemaTypeToXML "paypal-subscription"
 
data Pp_subscription_id = Pp_subscription_id
        deriving (Eq,Show)
instance SchemaType Pp_subscription_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Pp_subscription_id
    schemaTypeToXML s x@Pp_subscription_id{} =
        toXMLElement s []
            []
 
elementPp_subscription_id :: XMLParser Xsd.XsdString
elementPp_subscription_id = parseSchemaType "pp-subscription-id"
elementToXMLPp_subscription_id :: Xsd.XsdString -> [Content ()]
elementToXMLPp_subscription_id = schemaTypeToXML "pp-subscription-id"
 
data Pp_original_transaction_id = Pp_original_transaction_id
        deriving (Eq,Show)
instance SchemaType Pp_original_transaction_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Pp_original_transaction_id
    schemaTypeToXML s x@Pp_original_transaction_id{} =
        toXMLElement s []
            []
 
elementPp_original_transaction_id :: XMLParser Xsd.XsdString
elementPp_original_transaction_id = parseSchemaType "pp-original-transaction-id"
elementToXMLPp_original_transaction_id :: Xsd.XsdString -> [Content ()]
elementToXMLPp_original_transaction_id = schemaTypeToXML "pp-original-transaction-id"
 
data Value = Value
        deriving (Eq,Show)
instance SchemaType Value where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Value
    schemaTypeToXML s x@Value{} =
        toXMLElement s []
            []
 
elementValue :: XMLParser Xsd.Decimal
elementValue = parseSchemaType "value"
elementToXMLValue :: Xsd.Decimal -> [Content ()]
elementToXMLValue = schemaTypeToXML "value"
 
data Symbol = Symbol
        deriving (Eq,Show)
instance SchemaType Symbol where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Symbol
    schemaTypeToXML s x@Symbol{} =
        toXMLElement s []
            []
 
elementSymbol :: XMLParser Xsd.XsdString
elementSymbol = parseSchemaType "symbol"
elementToXMLSymbol :: Xsd.XsdString -> [Content ()]
elementToXMLSymbol = schemaTypeToXML "symbol"
 
data Price = Price
        { price_value :: Xsd.Decimal
        , price_currency :: Xsd.XsdString
        , price_formatted_price :: Maybe Formatted_price
        }
        deriving (Eq,Show)
instance SchemaType Price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Price
            `apply` elementValue
            `apply` elementCurrency
            `apply` optional (elementFormatted_price)
    schemaTypeToXML s x@Price{} =
        toXMLElement s []
            [ elementToXMLValue $ price_value x
            , elementToXMLCurrency $ price_currency x
            , maybe [] (elementToXMLFormatted_price) $ price_formatted_price x
            ]
 
elementPrice :: XMLParser Price
elementPrice = parseSchemaType "price"
elementToXMLPrice :: Price -> [Content ()]
elementToXMLPrice = schemaTypeToXML "price"
 
data Iso = Iso
        deriving (Eq,Show)
instance SchemaType Iso where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Iso
    schemaTypeToXML s x@Iso{} =
        toXMLElement s []
            []
 
elementIso :: XMLParser Xsd.XsdString
elementIso = parseSchemaType "iso"
elementToXMLIso :: Xsd.XsdString -> [Content ()]
elementToXMLIso = schemaTypeToXML "iso"
 
data Formatted_price = Formatted_price
        { formatted_price_iso :: Xsd.XsdString
        , formatted_price_symbol :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Formatted_price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Formatted_price
            `apply` elementIso
            `apply` elementSymbol
    schemaTypeToXML s x@Formatted_price{} =
        toXMLElement s []
            [ elementToXMLIso $ formatted_price_iso x
            , elementToXMLSymbol $ formatted_price_symbol x
            ]
 
elementFormatted_price :: XMLParser Formatted_price
elementFormatted_price = parseSchemaType "formatted-price"
elementToXMLFormatted_price :: Formatted_price -> [Content ()]
elementToXMLFormatted_price = schemaTypeToXML "formatted-price"
 
data Amount = Amount
        deriving (Eq,Show)
instance SchemaType Amount where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Amount
    schemaTypeToXML s x@Amount{} =
        toXMLElement s []
            []
 
elementAmount :: XMLParser Xsd.Decimal
elementAmount = parseSchemaType "amount"
elementToXMLAmount :: Xsd.Decimal -> [Content ()]
elementToXMLAmount = schemaTypeToXML "amount"
 
data Currency = Currency
        deriving (Eq,Show)
instance SchemaType Currency where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Currency
    schemaTypeToXML s x@Currency{} =
        toXMLElement s []
            []
 
elementCurrency :: XMLParser Xsd.XsdString
elementCurrency = parseSchemaType "currency"
elementToXMLCurrency :: Xsd.XsdString -> [Content ()]
elementToXMLCurrency = schemaTypeToXML "currency"
 
data Product = Product
        { product_product_id :: Maybe Xs.Long
        , product_product_status :: Xsd.XsdString
        , product_product_name :: Xsd.XsdString
        , product_product_short_description :: Xsd.XsdString
        , product_product_long_description :: Xsd.XsdString
        , product_product_info_url :: Xsd.XsdString
        , product_product_image :: Xsd.XsdString
        , product_product_merchant_descriptor :: Xsd.XsdString
        , product_product_support_email :: Xsd.XsdString
        , product_product_skus :: Product_skus
        }
        deriving (Eq,Show)
instance SchemaType Product where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Product
            `apply` optional (elementProduct_id)
            `apply` elementProduct_status
            `apply` elementProduct_name
            `apply` elementProduct_short_description
            `apply` elementProduct_long_description
            `apply` elementProduct_info_url
            `apply` elementProduct_image
            `apply` elementProduct_merchant_descriptor
            `apply` elementProduct_support_email
            `apply` elementProduct_skus
    schemaTypeToXML s x@Product{} =
        toXMLElement s []
            [ maybe [] (elementToXMLProduct_id) $ product_product_id x
            , elementToXMLProduct_status $ product_product_status x
            , elementToXMLProduct_name $ product_product_name x
            , elementToXMLProduct_short_description $ product_product_short_description x
            , elementToXMLProduct_long_description $ product_product_long_description x
            , elementToXMLProduct_info_url $ product_product_info_url x
            , elementToXMLProduct_image $ product_product_image x
            , elementToXMLProduct_merchant_descriptor $ product_product_merchant_descriptor x
            , elementToXMLProduct_support_email $ product_product_support_email x
            , elementToXMLProduct_skus $ product_product_skus x
            ]
 
elementProduct :: XMLParser Product
elementProduct = parseSchemaType "product"
elementToXMLProduct :: Product -> [Content ()]
elementToXMLProduct = schemaTypeToXML "product"
 
data Product_skus = Product_skus
        { product_skus_url :: [Xsd.XsdString]
        }
        deriving (Eq,Show)
instance SchemaType Product_skus where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Product_skus
            `apply` many (elementUrl)
    schemaTypeToXML s x@Product_skus{} =
        toXMLElement s []
            [ concatMap (elementToXMLUrl) $ product_skus_url x
            ]
 
elementProduct_skus :: XMLParser Product_skus
elementProduct_skus = parseSchemaType "product-skus"
elementToXMLProduct_skus :: Product_skus -> [Content ()]
elementToXMLProduct_skus = schemaTypeToXML "product-skus"
 
data Product_id = Product_id
        deriving (Eq,Show)
instance SchemaType Product_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Product_id
    schemaTypeToXML s x@Product_id{} =
        toXMLElement s []
            []
 
elementProduct_id :: XMLParser Xs.Long
elementProduct_id = parseSchemaType "product-id"
elementToXMLProduct_id :: Xs.Long -> [Content ()]
elementToXMLProduct_id = schemaTypeToXML "product-id"
 
data Product_name = Product_name
        deriving (Eq,Show)
instance SchemaType Product_name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Product_name
    schemaTypeToXML s x@Product_name{} =
        toXMLElement s []
            []
 
elementProduct_name :: XMLParser Xsd.XsdString
elementProduct_name = parseSchemaType "product-name"
elementToXMLProduct_name :: Xsd.XsdString -> [Content ()]
elementToXMLProduct_name = schemaTypeToXML "product-name"
 
data Product_short_description = Product_short_description
        deriving (Eq,Show)
instance SchemaType Product_short_description where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Product_short_description
    schemaTypeToXML s x@Product_short_description{} =
        toXMLElement s []
            []
 
elementProduct_short_description :: XMLParser Xsd.XsdString
elementProduct_short_description = parseSchemaType "product-short-description"
elementToXMLProduct_short_description :: Xsd.XsdString -> [Content ()]
elementToXMLProduct_short_description = schemaTypeToXML "product-short-description"
 
data Product_long_description = Product_long_description
        deriving (Eq,Show)
instance SchemaType Product_long_description where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Product_long_description
    schemaTypeToXML s x@Product_long_description{} =
        toXMLElement s []
            []
 
elementProduct_long_description :: XMLParser Xsd.XsdString
elementProduct_long_description = parseSchemaType "product-long-description"
elementToXMLProduct_long_description :: Xsd.XsdString -> [Content ()]
elementToXMLProduct_long_description = schemaTypeToXML "product-long-description"
 
data Product_info_url = Product_info_url
        deriving (Eq,Show)
instance SchemaType Product_info_url where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Product_info_url
    schemaTypeToXML s x@Product_info_url{} =
        toXMLElement s []
            []
 
elementProduct_info_url :: XMLParser Xsd.XsdString
elementProduct_info_url = parseSchemaType "product-info-url"
elementToXMLProduct_info_url :: Xsd.XsdString -> [Content ()]
elementToXMLProduct_info_url = schemaTypeToXML "product-info-url"
 
data Product_image = Product_image
        deriving (Eq,Show)
instance SchemaType Product_image where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Product_image
    schemaTypeToXML s x@Product_image{} =
        toXMLElement s []
            []
 
elementProduct_image :: XMLParser Xsd.XsdString
elementProduct_image = parseSchemaType "product-image"
elementToXMLProduct_image :: Xsd.XsdString -> [Content ()]
elementToXMLProduct_image = schemaTypeToXML "product-image"
 
data Product_merchant_descriptor = Product_merchant_descriptor
        deriving (Eq,Show)
instance SchemaType Product_merchant_descriptor where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Product_merchant_descriptor
    schemaTypeToXML s x@Product_merchant_descriptor{} =
        toXMLElement s []
            []
 
elementProduct_merchant_descriptor :: XMLParser Xsd.XsdString
elementProduct_merchant_descriptor = parseSchemaType "product-merchant-descriptor"
elementToXMLProduct_merchant_descriptor :: Xsd.XsdString -> [Content ()]
elementToXMLProduct_merchant_descriptor = schemaTypeToXML "product-merchant-descriptor"
 
data Product_support_email = Product_support_email
        deriving (Eq,Show)
instance SchemaType Product_support_email where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Product_support_email
    schemaTypeToXML s x@Product_support_email{} =
        toXMLElement s []
            []
 
elementProduct_support_email :: XMLParser Xsd.XsdString
elementProduct_support_email = parseSchemaType "product-support-email"
elementToXMLProduct_support_email :: Xsd.XsdString -> [Content ()]
elementToXMLProduct_support_email = schemaTypeToXML "product-support-email"
 
data Product_status = Product_status
        deriving (Eq,Show)
instance SchemaType Product_status where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Product_status
    schemaTypeToXML s x@Product_status{} =
        toXMLElement s []
            []
 
elementProduct_status :: XMLParser Xsd.XsdString
elementProduct_status = parseSchemaType "product-status"
elementToXMLProduct_status :: Xsd.XsdString -> [Content ()]
elementToXMLProduct_status = schemaTypeToXML "product-status"
 
data Shopper_subscriptions = Shopper_subscriptions
        { shopper_subscriptions_ordering_shopper :: Ordering_shopper
        , shopper_subscriptions_subscriptions :: Subscriptions
        }
        deriving (Eq,Show)
instance SchemaType Shopper_subscriptions where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Shopper_subscriptions
            `apply` elementOrdering_shopper
            `apply` elementSubscriptions
    schemaTypeToXML s x@Shopper_subscriptions{} =
        toXMLElement s []
            [ elementToXMLOrdering_shopper $ shopper_subscriptions_ordering_shopper x
            , elementToXMLSubscriptions $ shopper_subscriptions_subscriptions x
            ]
 
elementShopper_subscriptions :: XMLParser Shopper_subscriptions
elementShopper_subscriptions = parseSchemaType "shopper-subscriptions"
elementToXMLShopper_subscriptions :: Shopper_subscriptions -> [Content ()]
elementToXMLShopper_subscriptions = schemaTypeToXML "shopper-subscriptions"
 
data Subscriptions = Subscriptions
        { subscriptions_url :: [Xsd.XsdString]
        , subscriptions_subscription :: [Subscription]
        }
        deriving (Eq,Show)
instance SchemaType Subscriptions where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Subscriptions
            `apply` many (elementUrl)
            `apply` many (elementSubscription)
    schemaTypeToXML s x@Subscriptions{} =
        toXMLElement s []
            [ concatMap (elementToXMLUrl) $ subscriptions_url x
            , concatMap (elementToXMLSubscription) $ subscriptions_subscription x
            ]
 
elementSubscriptions :: XMLParser Subscriptions
elementSubscriptions = parseSchemaType "subscriptions"
elementToXMLSubscriptions :: Subscriptions -> [Content ()]
elementToXMLSubscriptions = schemaTypeToXML "subscriptions"
 
data Vat_code = Vat_code
        deriving (Eq,Show)
instance SchemaType Vat_code where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Vat_code
    schemaTypeToXML s x@Vat_code{} =
        toXMLElement s []
            []
 
elementVat_code :: XMLParser Xsd.XsdString
elementVat_code = parseSchemaType "vat-code"
elementToXMLVat_code :: Xsd.XsdString -> [Content ()]
elementToXMLVat_code = schemaTypeToXML "vat-code"
 
data Shopper_currency = Shopper_currency
        deriving (Eq,Show)
instance SchemaType Shopper_currency where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Shopper_currency
    schemaTypeToXML s x@Shopper_currency{} =
        toXMLElement s []
            []
 
elementShopper_currency :: XMLParser Xsd.XsdString
elementShopper_currency = parseSchemaType "shopper-currency"
elementToXMLShopper_currency :: Xsd.XsdString -> [Content ()]
elementToXMLShopper_currency = schemaTypeToXML "shopper-currency"
 
data Locale = Locale
        deriving (Eq,Show)
instance SchemaType Locale where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Locale
    schemaTypeToXML s x@Locale{} =
        toXMLElement s []
            []
 
elementLocale :: XMLParser Xsd.XsdString
elementLocale = parseSchemaType "locale"
elementToXMLLocale :: Xsd.XsdString -> [Content ()]
elementToXMLLocale = schemaTypeToXML "locale"
 
data Permitted_future_charges = Permitted_future_charges
        deriving (Eq,Show)
instance SchemaType Permitted_future_charges where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Permitted_future_charges
    schemaTypeToXML s x@Permitted_future_charges{} =
        toXMLElement s []
            []
 
elementPermitted_future_charges :: XMLParser Xsd.Boolean
elementPermitted_future_charges = parseSchemaType "permitted-future-charges"
elementToXMLPermitted_future_charges :: Xsd.Boolean -> [Content ()]
elementToXMLPermitted_future_charges = schemaTypeToXML "permitted-future-charges"
 
data Title = Title
        deriving (Eq,Show)
instance SchemaType Title where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Title
    schemaTypeToXML s x@Title{} =
        toXMLElement s []
            []
 
elementTitle :: XMLParser Xsd.XsdString
elementTitle = parseSchemaType "title"
elementToXMLTitle :: Xsd.XsdString -> [Content ()]
elementToXMLTitle = schemaTypeToXML "title"
 
data Username = Username
        deriving (Eq,Show)
instance SchemaType Username where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Username
    schemaTypeToXML s x@Username{} =
        toXMLElement s []
            []
 
elementUsername :: XMLParser Xsd.XsdString
elementUsername = parseSchemaType "username"
elementToXMLUsername :: Xsd.XsdString -> [Content ()]
elementToXMLUsername = schemaTypeToXML "username"
 
data Password = Password
        deriving (Eq,Show)
instance SchemaType Password where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Password
    schemaTypeToXML s x@Password{} =
        toXMLElement s []
            []
 
elementPassword :: XMLParser Xsd.XsdString
elementPassword = parseSchemaType "password"
elementToXMLPassword :: Xsd.XsdString -> [Content ()]
elementToXMLPassword = schemaTypeToXML "password"
 
data Shopper_info = Shopper_info
        { shopper_info_shopper_id :: Maybe Xs.Long
        , shopper_info_seller_shopper_id :: Maybe Xsd.XsdString
        , shopper_info_username :: [Xsd.XsdString]
        , shopper_info_password :: [Xsd.XsdString]
        , shopper_info_shopper_contact_info :: Maybe Shopper_contact_info
        , shopper_info_shipping_contact_info :: Maybe Shipping_contact_info
        , shopper_info_invoice_contacts_info :: [Invoice_contacts_info]
        , shopper_info_payment_info :: Maybe Payment_info
        , shopper_info_store_id :: Maybe Xs.Long
        , shopper_info_vat_code :: Maybe Xsd.XsdString
        , shopper_info_shopper_currency :: Maybe Xsd.XsdString
        , shopper_info_locale :: Maybe Xsd.XsdString
        , shopper_info_permitted_future_charges :: [Xsd.Boolean]
        }
        deriving (Eq,Show)
instance SchemaType Shopper_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Shopper_info
            `apply` optional (elementShopper_id)
            `apply` optional (elementSeller_shopper_id)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementUsername)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementPassword)
            `apply` optional (elementShopper_contact_info)
            `apply` optional (elementShipping_contact_info)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementInvoice_contacts_info)
            `apply` optional (elementPayment_info)
            `apply` optional (elementStore_id)
            `apply` optional (elementVat_code)
            `apply` optional (elementShopper_currency)
            `apply` optional (elementLocale)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementPermitted_future_charges)
    schemaTypeToXML s x@Shopper_info{} =
        toXMLElement s []
            [ maybe [] (elementToXMLShopper_id) $ shopper_info_shopper_id x
            , maybe [] (elementToXMLSeller_shopper_id) $ shopper_info_seller_shopper_id x
            , concatMap (elementToXMLUsername) $ shopper_info_username x
            , concatMap (elementToXMLPassword) $ shopper_info_password x
            , maybe [] (elementToXMLShopper_contact_info) $ shopper_info_shopper_contact_info x
            , maybe [] (elementToXMLShipping_contact_info) $ shopper_info_shipping_contact_info x
            , concatMap (elementToXMLInvoice_contacts_info) $ shopper_info_invoice_contacts_info x
            , maybe [] (elementToXMLPayment_info) $ shopper_info_payment_info x
            , maybe [] (elementToXMLStore_id) $ shopper_info_store_id x
            , maybe [] (elementToXMLVat_code) $ shopper_info_vat_code x
            , maybe [] (elementToXMLShopper_currency) $ shopper_info_shopper_currency x
            , maybe [] (elementToXMLLocale) $ shopper_info_locale x
            , concatMap (elementToXMLPermitted_future_charges) $ shopper_info_permitted_future_charges x
            ]
 
elementShopper_info :: XMLParser Shopper_info
elementShopper_info = parseSchemaType "shopper-info"
elementToXMLShopper_info :: Shopper_info -> [Content ()]
elementToXMLShopper_info = schemaTypeToXML "shopper-info"
 
data Shopper_contact_info = Shopper_contact_info
        { shopper_contact_info_title :: Maybe Xsd.XsdString
        , shopper_contact_info_first_name :: Maybe Xsd.XsdString
        , shopper_contact_info_last_name :: Maybe Xsd.XsdString
        , shopper_contact_info_email :: Maybe Xsd.XsdString
        , shopper_contact_info_company_name :: Maybe Xsd.XsdString
        , shopper_contact_info_address1 :: Maybe Xsd.XsdString
        , shopper_contact_info_address2 :: Maybe Xsd.XsdString
        , shopper_contact_info_city :: Maybe Xsd.XsdString
        , shopper_contact_info_state :: Maybe Xsd.XsdString
        , shopper_contact_info_zip :: Maybe Xsd.XsdString
        , shopper_contact_info_country :: Maybe Xsd.XsdString
        , shopper_contact_info_phone :: Maybe Xsd.XsdString
        , shopper_contact_info_fax :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Shopper_contact_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Shopper_contact_info
            `apply` optional (elementTitle)
            `apply` optional (elementFirst_name)
            `apply` optional (elementLast_name)
            `apply` optional (elementEmail)
            `apply` optional (elementCompany_name)
            `apply` optional (elementAddress1)
            `apply` optional (elementAddress2)
            `apply` optional (elementCity)
            `apply` optional (elementState)
            `apply` optional (elementZip)
            `apply` optional (elementCountry)
            `apply` optional (elementPhone)
            `apply` optional (elementFax)
    schemaTypeToXML s x@Shopper_contact_info{} =
        toXMLElement s []
            [ maybe [] (elementToXMLTitle) $ shopper_contact_info_title x
            , maybe [] (elementToXMLFirst_name) $ shopper_contact_info_first_name x
            , maybe [] (elementToXMLLast_name) $ shopper_contact_info_last_name x
            , maybe [] (elementToXMLEmail) $ shopper_contact_info_email x
            , maybe [] (elementToXMLCompany_name) $ shopper_contact_info_company_name x
            , maybe [] (elementToXMLAddress1) $ shopper_contact_info_address1 x
            , maybe [] (elementToXMLAddress2) $ shopper_contact_info_address2 x
            , maybe [] (elementToXMLCity) $ shopper_contact_info_city x
            , maybe [] (elementToXMLState) $ shopper_contact_info_state x
            , maybe [] (elementToXMLZip) $ shopper_contact_info_zip x
            , maybe [] (elementToXMLCountry) $ shopper_contact_info_country x
            , maybe [] (elementToXMLPhone) $ shopper_contact_info_phone x
            , maybe [] (elementToXMLFax) $ shopper_contact_info_fax x
            ]
 
elementShopper_contact_info :: XMLParser Shopper_contact_info
elementShopper_contact_info = parseSchemaType "shopper-contact-info"
elementToXMLShopper_contact_info :: Shopper_contact_info -> [Content ()]
elementToXMLShopper_contact_info = schemaTypeToXML "shopper-contact-info"
 
data Shopper = Shopper
        { shopper_shopper_info :: Shopper_info
        , shopper_web_info :: [Web_info]
        , shopper_three_d_authenticated_info :: [Three_d_authenticated_info]
        }
        deriving (Eq,Show)
instance SchemaType Shopper where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Shopper
            `apply` elementShopper_info
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementWeb_info)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementThree_d_authenticated_info)
    schemaTypeToXML s x@Shopper{} =
        toXMLElement s []
            [ elementToXMLShopper_info $ shopper_shopper_info x
            , concatMap (elementToXMLWeb_info) $ shopper_web_info x
            , concatMap (elementToXMLThree_d_authenticated_info) $ shopper_three_d_authenticated_info x
            ]
 
elementShopper :: XMLParser Shopper
elementShopper = parseSchemaType "shopper"
elementToXMLShopper :: Shopper -> [Content ()]
elementToXMLShopper = schemaTypeToXML "shopper"
 
data Shipping_contact_info = Shipping_contact_info
        { shipping_contact_info_first_name :: Maybe Xsd.XsdString
        , shipping_contact_info_last_name :: Maybe Xsd.XsdString
        , shipping_contact_info_address1 :: Maybe Xsd.XsdString
        , shipping_contact_info_address2 :: Maybe Xsd.XsdString
        , shipping_contact_info_city :: Maybe Xsd.XsdString
        , shipping_contact_info_state :: Maybe Xsd.XsdString
        , shipping_contact_info_zip :: Maybe Xsd.XsdString
        , shipping_contact_info_country :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Shipping_contact_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Shipping_contact_info
            `apply` optional (elementFirst_name)
            `apply` optional (elementLast_name)
            `apply` optional (elementAddress1)
            `apply` optional (elementAddress2)
            `apply` optional (elementCity)
            `apply` optional (elementState)
            `apply` optional (elementZip)
            `apply` optional (elementCountry)
    schemaTypeToXML s x@Shipping_contact_info{} =
        toXMLElement s []
            [ maybe [] (elementToXMLFirst_name) $ shipping_contact_info_first_name x
            , maybe [] (elementToXMLLast_name) $ shipping_contact_info_last_name x
            , maybe [] (elementToXMLAddress1) $ shipping_contact_info_address1 x
            , maybe [] (elementToXMLAddress2) $ shipping_contact_info_address2 x
            , maybe [] (elementToXMLCity) $ shipping_contact_info_city x
            , maybe [] (elementToXMLState) $ shipping_contact_info_state x
            , maybe [] (elementToXMLZip) $ shipping_contact_info_zip x
            , maybe [] (elementToXMLCountry) $ shipping_contact_info_country x
            ]
 
elementShipping_contact_info :: XMLParser Shipping_contact_info
elementShipping_contact_info = parseSchemaType "shipping-contact-info"
elementToXMLShipping_contact_info :: Shipping_contact_info -> [Content ()]
elementToXMLShipping_contact_info = schemaTypeToXML "shipping-contact-info"
 
data Invoice_contacts_info = Invoice_contacts_info
        { invoice_contacts_info_invoice_contact_info :: [Invoice_contact_info]
        }
        deriving (Eq,Show)
instance SchemaType Invoice_contacts_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Invoice_contacts_info
            `apply` many (elementInvoice_contact_info)
    schemaTypeToXML s x@Invoice_contacts_info{} =
        toXMLElement s []
            [ concatMap (elementToXMLInvoice_contact_info) $ invoice_contacts_info_invoice_contact_info x
            ]
 
elementInvoice_contacts_info :: XMLParser Invoice_contacts_info
elementInvoice_contacts_info = parseSchemaType "invoice-contacts-info"
elementToXMLInvoice_contacts_info :: Invoice_contacts_info -> [Content ()]
elementToXMLInvoice_contacts_info = schemaTypeToXML "invoice-contacts-info"
 
data Invoice_contact_info = Invoice_contact_info
        { invoice_contact_info_default :: Maybe Xsd.Boolean
        , invoice_contact_info_vat_code :: Maybe Xsd.XsdString
        , invoice_contact_info_title :: Maybe Xsd.XsdString
        , invoice_contact_info_first_name :: Maybe Xsd.XsdString
        , invoice_contact_info_last_name :: Maybe Xsd.XsdString
        , invoice_contact_info_email :: Maybe Xsd.XsdString
        , invoice_contact_info_company_name :: Maybe Xsd.XsdString
        , invoice_contact_info_address1 :: Maybe Xsd.XsdString
        , invoice_contact_info_address2 :: Maybe Xsd.XsdString
        , invoice_contact_info_city :: Maybe Xsd.XsdString
        , invoice_contact_info_state :: Maybe Xsd.XsdString
        , invoice_contact_info_zip :: Maybe Xsd.XsdString
        , invoice_contact_info_country :: Maybe Xsd.XsdString
        , invoice_contact_info_phone :: Maybe Xsd.XsdString
        , invoice_contact_info_fax :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Invoice_contact_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Invoice_contact_info
            `apply` optional (elementDefault)
            `apply` optional (elementVat_code)
            `apply` optional (elementTitle)
            `apply` optional (elementFirst_name)
            `apply` optional (elementLast_name)
            `apply` optional (elementEmail)
            `apply` optional (elementCompany_name)
            `apply` optional (elementAddress1)
            `apply` optional (elementAddress2)
            `apply` optional (elementCity)
            `apply` optional (elementState)
            `apply` optional (elementZip)
            `apply` optional (elementCountry)
            `apply` optional (elementPhone)
            `apply` optional (elementFax)
    schemaTypeToXML s x@Invoice_contact_info{} =
        toXMLElement s []
            [ maybe [] (elementToXMLDefault) $ invoice_contact_info_default x
            , maybe [] (elementToXMLVat_code) $ invoice_contact_info_vat_code x
            , maybe [] (elementToXMLTitle) $ invoice_contact_info_title x
            , maybe [] (elementToXMLFirst_name) $ invoice_contact_info_first_name x
            , maybe [] (elementToXMLLast_name) $ invoice_contact_info_last_name x
            , maybe [] (elementToXMLEmail) $ invoice_contact_info_email x
            , maybe [] (elementToXMLCompany_name) $ invoice_contact_info_company_name x
            , maybe [] (elementToXMLAddress1) $ invoice_contact_info_address1 x
            , maybe [] (elementToXMLAddress2) $ invoice_contact_info_address2 x
            , maybe [] (elementToXMLCity) $ invoice_contact_info_city x
            , maybe [] (elementToXMLState) $ invoice_contact_info_state x
            , maybe [] (elementToXMLZip) $ invoice_contact_info_zip x
            , maybe [] (elementToXMLCountry) $ invoice_contact_info_country x
            , maybe [] (elementToXMLPhone) $ invoice_contact_info_phone x
            , maybe [] (elementToXMLFax) $ invoice_contact_info_fax x
            ]
 
elementInvoice_contact_info :: XMLParser Invoice_contact_info
elementInvoice_contact_info = parseSchemaType "invoice-contact-info"
elementToXMLInvoice_contact_info :: Invoice_contact_info -> [Content ()]
elementToXMLInvoice_contact_info = schemaTypeToXML "invoice-contact-info"
 
data Default = Default
        deriving (Eq,Show)
instance SchemaType Default where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Default
    schemaTypeToXML s x@Default{} =
        toXMLElement s []
            []
 
elementDefault :: XMLParser Xsd.Boolean
elementDefault = parseSchemaType "default"
elementToXMLDefault :: Xsd.Boolean -> [Content ()]
elementToXMLDefault = schemaTypeToXML "default"
 
data Phone = Phone
        deriving (Eq,Show)
instance SchemaType Phone where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Phone
    schemaTypeToXML s x@Phone{} =
        toXMLElement s []
            []
 
elementPhone :: XMLParser Xsd.XsdString
elementPhone = parseSchemaType "phone"
elementToXMLPhone :: Xsd.XsdString -> [Content ()]
elementToXMLPhone = schemaTypeToXML "phone"
 
data Payment_info = Payment_info
        { payment_info_credit_card_info :: Maybe Credit_card_info
        , payment_info_credit_cards_info :: [Credit_cards_info]
        , payment_info_ecps_info :: [Ecps_info]
        , payment_info_balance :: [Balance]
        }
        deriving (Eq,Show)
instance SchemaType Payment_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Payment_info
            `apply` optional (elementCredit_card_info)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementCredit_cards_info)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementEcps_info)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementBalance)
    schemaTypeToXML s x@Payment_info{} =
        toXMLElement s []
            [ maybe [] (elementToXMLCredit_card_info) $ payment_info_credit_card_info x
            , concatMap (elementToXMLCredit_cards_info) $ payment_info_credit_cards_info x
            , concatMap (elementToXMLEcps_info) $ payment_info_ecps_info x
            , concatMap (elementToXMLBalance) $ payment_info_balance x
            ]
 
elementPayment_info :: XMLParser Payment_info
elementPayment_info = parseSchemaType "payment-info"
elementToXMLPayment_info :: Payment_info -> [Content ()]
elementToXMLPayment_info = schemaTypeToXML "payment-info"
 
data Credit_cards_info = Credit_cards_info
        { credit_cards_info_credit_card_info :: [Credit_card_info]
        }
        deriving (Eq,Show)
instance SchemaType Credit_cards_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Credit_cards_info
            `apply` many (elementCredit_card_info)
    schemaTypeToXML s x@Credit_cards_info{} =
        toXMLElement s []
            [ concatMap (elementToXMLCredit_card_info) $ credit_cards_info_credit_card_info x
            ]
 
elementCredit_cards_info :: XMLParser Credit_cards_info
elementCredit_cards_info = parseSchemaType "credit-cards-info"
elementToXMLCredit_cards_info :: Credit_cards_info -> [Content ()]
elementToXMLCredit_cards_info = schemaTypeToXML "credit-cards-info"
 
data Balance = Balance
        { balance_currency :: Xsd.XsdString
        , balance_value :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Balance where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Balance
            `apply` elementCurrency
            `apply` elementValue
    schemaTypeToXML s x@Balance{} =
        toXMLElement s []
            [ elementToXMLCurrency $ balance_currency x
            , elementToXMLValue $ balance_value x
            ]
 
elementBalance :: XMLParser Balance
elementBalance = parseSchemaType "balance"
elementToXMLBalance :: Balance -> [Content ()]
elementToXMLBalance = schemaTypeToXML "balance"
 
data Id = Id
        deriving (Eq,Show)
instance SchemaType Id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Id
    schemaTypeToXML s x@Id{} =
        toXMLElement s []
            []
 
elementId :: XMLParser Xs.Long
elementId = parseSchemaType "id"
elementToXMLId :: Xs.Long -> [Content ()]
elementToXMLId = schemaTypeToXML "id"
 
data Fax = Fax
        deriving (Eq,Show)
instance SchemaType Fax where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Fax
    schemaTypeToXML s x@Fax{} =
        toXMLElement s []
            []
 
elementFax :: XMLParser Xsd.XsdString
elementFax = parseSchemaType "fax"
elementToXMLFax :: Xsd.XsdString -> [Content ()]
elementToXMLFax = schemaTypeToXML "fax"
 
data Email = Email
        deriving (Eq,Show)
instance SchemaType Email where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Email
    schemaTypeToXML s x@Email{} =
        toXMLElement s []
            []
 
elementEmail :: XMLParser Xsd.XsdString
elementEmail = parseSchemaType "email"
elementToXMLEmail :: Xsd.XsdString -> [Content ()]
elementToXMLEmail = schemaTypeToXML "email"
 
data Company_name = Company_name
        deriving (Eq,Show)
instance SchemaType Company_name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Company_name
    schemaTypeToXML s x@Company_name{} =
        toXMLElement s []
            []
 
elementCompany_name :: XMLParser Xsd.XsdString
elementCompany_name = parseSchemaType "company-name"
elementToXMLCompany_name :: Xsd.XsdString -> [Content ()]
elementToXMLCompany_name = schemaTypeToXML "company-name"
 
data Step = Step
        deriving (Eq,Show)
instance SchemaType Step where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Step
    schemaTypeToXML s x@Step{} =
        toXMLElement s []
            []
 
elementStep :: XMLParser Xsd.XsdString
elementStep = parseSchemaType "step"
elementToXMLStep :: Xsd.XsdString -> [Content ()]
elementToXMLStep = schemaTypeToXML "step"
 
data Buynow_sso_token = Buynow_sso_token
        deriving (Eq,Show)
instance SchemaType Buynow_sso_token where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Buynow_sso_token
    schemaTypeToXML s x@Buynow_sso_token{} =
        toXMLElement s []
            []
 
elementBuynow_sso_token :: XMLParser Xsd.XsdString
elementBuynow_sso_token = parseSchemaType "buynow-sso-token"
elementToXMLBuynow_sso_token :: Xsd.XsdString -> [Content ()]
elementToXMLBuynow_sso_token = schemaTypeToXML "buynow-sso-token"
 
data Shopper_details = Shopper_details
        { shopper_details_shopper :: Shopper
        }
        deriving (Eq,Show)
instance SchemaType Shopper_details where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Shopper_details
            `apply` elementShopper
    schemaTypeToXML s x@Shopper_details{} =
        toXMLElement s []
            [ elementToXMLShopper $ shopper_details_shopper x
            ]
 
elementShopper_details :: XMLParser Shopper_details
elementShopper_details = parseSchemaType "shopper-details"
elementToXMLShopper_details :: Shopper_details -> [Content ()]
elementToXMLShopper_details = schemaTypeToXML "shopper-details"
 
data Order_details = Order_details
        { order_details_order :: Maybe Order
        , order_details_order_id :: Maybe Xs.Long
        }
        deriving (Eq,Show)
instance SchemaType Order_details where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Order_details
            `apply` optional (elementOrder)
            `apply` optional (elementOrder_id)
    schemaTypeToXML s x@Order_details{} =
        toXMLElement s []
            [ maybe [] (elementToXMLOrder) $ order_details_order x
            , maybe [] (elementToXMLOrder_id) $ order_details_order_id x
            ]
 
elementOrder_details :: XMLParser Order_details
elementOrder_details = parseSchemaType "order-details"
elementToXMLOrder_details :: Order_details -> [Content ()]
elementToXMLOrder_details = schemaTypeToXML "order-details"
 
data Shopping_context = Shopping_context
        { shopping_context_step :: Xsd.XsdString
        , shopping_context_buynow_sso_token :: Maybe Xsd.XsdString
        , shopping_context_web_info :: Web_info
        , shopping_context_shopper_details :: Maybe Shopper_details
        , shopping_context_order_details :: Order_details
        }
        deriving (Eq,Show)
instance SchemaType Shopping_context where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Shopping_context
            `apply` elementStep
            `apply` optional (elementBuynow_sso_token)
            `apply` elementWeb_info
            `apply` optional (elementShopper_details)
            `apply` elementOrder_details
    schemaTypeToXML s x@Shopping_context{} =
        toXMLElement s []
            [ elementToXMLStep $ shopping_context_step x
            , maybe [] (elementToXMLBuynow_sso_token) $ shopping_context_buynow_sso_token x
            , elementToXMLWeb_info $ shopping_context_web_info x
            , maybe [] (elementToXMLShopper_details) $ shopping_context_shopper_details x
            , elementToXMLOrder_details $ shopping_context_order_details x
            ]
 
elementShopping_context :: XMLParser Shopping_context
elementShopping_context = parseSchemaType "shopping-context"
elementToXMLShopping_context :: Shopping_context -> [Content ()]
elementToXMLShopping_context = schemaTypeToXML "shopping-context"
 
data Sku = Sku
        { sku_sku_id :: Xs.Long
        , sku_sku_name :: Maybe Xsd.XsdString
        , sku_sku_charge_price :: [Sku_charge_price]
        }
        deriving (Eq,Show)
instance SchemaType Sku where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku
            `apply` elementSku_id
            `apply` optional (elementSku_name)
            `apply` between (Occurs (Just 0) (Just 2))
                            (elementSku_charge_price)
    schemaTypeToXML s x@Sku{} =
        toXMLElement s []
            [ elementToXMLSku_id $ sku_sku_id x
            , maybe [] (elementToXMLSku_name) $ sku_sku_name x
            , concatMap (elementToXMLSku_charge_price) $ sku_sku_charge_price x
            ]
 
elementSku :: XMLParser Sku
elementSku = parseSchemaType "sku"
elementToXMLSku :: Sku -> [Content ()]
elementToXMLSku = schemaTypeToXML "sku"
 
data Sku_charge_price = Sku_charge_price
        { sku_charge_price_charge_type :: Xsd.XsdString
        , sku_charge_price_amount :: Xsd.Decimal
        , sku_charge_price_currency :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Sku_charge_price where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_charge_price
            `apply` elementCharge_type
            `apply` elementAmount
            `apply` elementCurrency
    schemaTypeToXML s x@Sku_charge_price{} =
        toXMLElement s []
            [ elementToXMLCharge_type $ sku_charge_price_charge_type x
            , elementToXMLAmount $ sku_charge_price_amount x
            , elementToXMLCurrency $ sku_charge_price_currency x
            ]
 
elementSku_charge_price :: XMLParser Sku_charge_price
elementSku_charge_price = parseSchemaType "sku-charge-price"
elementToXMLSku_charge_price :: Sku_charge_price -> [Content ()]
elementToXMLSku_charge_price = schemaTypeToXML "sku-charge-price"
 
data Sku_id = Sku_id
        deriving (Eq,Show)
instance SchemaType Sku_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_id
    schemaTypeToXML s x@Sku_id{} =
        toXMLElement s []
            []
 
elementSku_id :: XMLParser Xs.Long
elementSku_id = parseSchemaType "sku-id"
elementToXMLSku_id :: Xs.Long -> [Content ()]
elementToXMLSku_id = schemaTypeToXML "sku-id"
 
data Sku_name = Sku_name
        deriving (Eq,Show)
instance SchemaType Sku_name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_name
    schemaTypeToXML s x@Sku_name{} =
        toXMLElement s []
            []
 
elementSku_name :: XMLParser Xsd.XsdString
elementSku_name = parseSchemaType "sku-name"
elementToXMLSku_name :: Xsd.XsdString -> [Content ()]
elementToXMLSku_name = schemaTypeToXML "sku-name"
 
data Contract_name = Contract_name
        deriving (Eq,Show)
instance SchemaType Contract_name where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Contract_name
    schemaTypeToXML s x@Contract_name{} =
        toXMLElement s []
            []
 
elementContract_name :: XMLParser Xsd.XsdString
elementContract_name = parseSchemaType "contract-name"
elementToXMLContract_name :: Xsd.XsdString -> [Content ()]
elementToXMLContract_name = schemaTypeToXML "contract-name"
 
data Sku_status = Sku_status
        deriving (Eq,Show)
instance SchemaType Sku_status where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Sku_status
    schemaTypeToXML s x@Sku_status{} =
        toXMLElement s []
            []
 
elementSku_status :: XMLParser Xsd.XsdString
elementSku_status = parseSchemaType "sku-status"
elementToXMLSku_status :: Xsd.XsdString -> [Content ()]
elementToXMLSku_status = schemaTypeToXML "sku-status"
 
data Subscription_charge = Subscription_charge
        { subscription_charge_charge_info :: Charge_info
        , subscription_charge_sku_charge_price :: Maybe Sku_charge_price
        , subscription_charge_expected_total_price :: Maybe Expected_total_price
        , subscription_charge_charge_invoice_info :: Maybe Charge_invoice_info
        }
        deriving (Eq,Show)
instance SchemaType Subscription_charge where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Subscription_charge
            `apply` elementCharge_info
            `apply` optional (elementSku_charge_price)
            `apply` optional (elementExpected_total_price)
            `apply` optional (elementCharge_invoice_info)
    schemaTypeToXML s x@Subscription_charge{} =
        toXMLElement s []
            [ elementToXMLCharge_info $ subscription_charge_charge_info x
            , maybe [] (elementToXMLSku_charge_price) $ subscription_charge_sku_charge_price x
            , maybe [] (elementToXMLExpected_total_price) $ subscription_charge_expected_total_price x
            , maybe [] (elementToXMLCharge_invoice_info) $ subscription_charge_charge_invoice_info x
            ]
 
elementSubscription_charge :: XMLParser Subscription_charge
elementSubscription_charge = parseSchemaType "subscription-charge"
elementToXMLSubscription_charge :: Subscription_charge -> [Content ()]
elementToXMLSubscription_charge = schemaTypeToXML "subscription-charge"
 
data Charge_info = Charge_info
        { charge_info_charge_description :: Xsd.XsdString
        , charge_info_from_date :: Maybe Xsd.XsdString
        , charge_info_to_date :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Charge_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Charge_info
            `apply` elementCharge_description
            `apply` optional (elementFrom_date)
            `apply` optional (elementTo_date)
    schemaTypeToXML s x@Charge_info{} =
        toXMLElement s []
            [ elementToXMLCharge_description $ charge_info_charge_description x
            , maybe [] (elementToXMLFrom_date) $ charge_info_from_date x
            , maybe [] (elementToXMLTo_date) $ charge_info_to_date x
            ]
 
elementCharge_info :: XMLParser Charge_info
elementCharge_info = parseSchemaType "charge-info"
elementToXMLCharge_info :: Charge_info -> [Content ()]
elementToXMLCharge_info = schemaTypeToXML "charge-info"
 
data Charge_invoice_info = Charge_invoice_info
        { charge_invoice_info_date_created :: Xsd.XsdString
        , charge_invoice_info_invoice_id :: Xs.Long
        , charge_invoice_info_invoice_amount :: Xsd.XsdString
        , charge_invoice_info_invoice_currency :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Charge_invoice_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Charge_invoice_info
            `apply` elementDate_created
            `apply` elementInvoice_id
            `apply` elementInvoice_amount
            `apply` elementInvoice_currency
    schemaTypeToXML s x@Charge_invoice_info{} =
        toXMLElement s []
            [ elementToXMLDate_created $ charge_invoice_info_date_created x
            , elementToXMLInvoice_id $ charge_invoice_info_invoice_id x
            , elementToXMLInvoice_amount $ charge_invoice_info_invoice_amount x
            , elementToXMLInvoice_currency $ charge_invoice_info_invoice_currency x
            ]
 
elementCharge_invoice_info :: XMLParser Charge_invoice_info
elementCharge_invoice_info = parseSchemaType "charge-invoice-info"
elementToXMLCharge_invoice_info :: Charge_invoice_info -> [Content ()]
elementToXMLCharge_invoice_info = schemaTypeToXML "charge-invoice-info"
 
data Charge_description = Charge_description
        deriving (Eq,Show)
instance SchemaType Charge_description where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Charge_description
    schemaTypeToXML s x@Charge_description{} =
        toXMLElement s []
            []
 
elementCharge_description :: XMLParser Xsd.XsdString
elementCharge_description = parseSchemaType "charge-description"
elementToXMLCharge_description :: Xsd.XsdString -> [Content ()]
elementToXMLCharge_description = schemaTypeToXML "charge-description"
 
data From_date = From_date
        deriving (Eq,Show)
instance SchemaType From_date where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return From_date
    schemaTypeToXML s x@From_date{} =
        toXMLElement s []
            []
 
elementFrom_date :: XMLParser Xsd.XsdString
elementFrom_date = parseSchemaType "from-date"
elementToXMLFrom_date :: Xsd.XsdString -> [Content ()]
elementToXMLFrom_date = schemaTypeToXML "from-date"
 
data To_date = To_date
        deriving (Eq,Show)
instance SchemaType To_date where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return To_date
    schemaTypeToXML s x@To_date{} =
        toXMLElement s []
            []
 
elementTo_date :: XMLParser Xsd.XsdString
elementTo_date = parseSchemaType "to-date"
elementToXMLTo_date :: Xsd.XsdString -> [Content ()]
elementToXMLTo_date = schemaTypeToXML "to-date"
 
data Invoice_amount = Invoice_amount
        deriving (Eq,Show)
instance SchemaType Invoice_amount where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Invoice_amount
    schemaTypeToXML s x@Invoice_amount{} =
        toXMLElement s []
            []
 
elementInvoice_amount :: XMLParser Xsd.XsdString
elementInvoice_amount = parseSchemaType "invoice-amount"
elementToXMLInvoice_amount :: Xsd.XsdString -> [Content ()]
elementToXMLInvoice_amount = schemaTypeToXML "invoice-amount"
 
data Invoice_currency = Invoice_currency
        deriving (Eq,Show)
instance SchemaType Invoice_currency where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Invoice_currency
    schemaTypeToXML s x@Invoice_currency{} =
        toXMLElement s []
            []
 
elementInvoice_currency :: XMLParser Xsd.XsdString
elementInvoice_currency = parseSchemaType "invoice-currency"
elementToXMLInvoice_currency :: Xsd.XsdString -> [Content ()]
elementToXMLInvoice_currency = schemaTypeToXML "invoice-currency"
 
data Subscription = Subscription
        { subscription_subscription_id :: Xs.Long
        , subscription_status :: Xsd.XsdString
        , subscription_cancellation_reason :: Maybe Xsd.XsdString
        , subscription_underlying_sku_id :: Xs.Long
        , subscription_shopper_id :: Xs.Long
        , subscription_recurring_charge :: Maybe Recurring_charge
        , subscription_credit_card :: Maybe Credit_card
        , subscription_ecp :: Maybe Ecp
        , subscription_paypal :: Maybe Paypal
        , subscription_paypal_subscription :: Maybe Paypal_subscription
        , subscription_catalog_recurring_charge :: Maybe Catalog_recurring_charge
        , subscription_override_recurring_charge :: Maybe Override_recurring_charge
        , subscription_coupon :: Maybe Xsd.XsdString
        , subscription_charge_frequency :: Maybe Xsd.XsdString
        , subscription_next_charge_date :: Xsd.XsdString
        , subscription_manual_recurring_info :: Maybe Manual_recurring_info
        , subscription_auto_renew :: Maybe Xsd.Boolean
        , subscription_last_charge_result :: Maybe Last_charge_result
        , subscription_subscription_charges :: Maybe Subscription_charges
        }
        deriving (Eq,Show)
instance SchemaType Subscription where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Subscription
            `apply` elementSubscription_id
            `apply` elementStatus
            `apply` optional (elementCancellation_reason)
            `apply` elementUnderlying_sku_id
            `apply` elementShopper_id
            `apply` optional (elementRecurring_charge)
            `apply` optional (elementCredit_card)
            `apply` optional (elementEcp)
            `apply` optional (elementPaypal)
            `apply` optional (elementPaypal_subscription)
            `apply` optional (elementCatalog_recurring_charge)
            `apply` optional (elementOverride_recurring_charge)
            `apply` optional (elementCoupon)
            `apply` optional (elementCharge_frequency)
            `apply` elementNext_charge_date
            `apply` optional (elementManual_recurring_info)
            `apply` optional (elementAuto_renew)
            `apply` optional (elementLast_charge_result)
            `apply` optional (elementSubscription_charges)
    schemaTypeToXML s x@Subscription{} =
        toXMLElement s []
            [ elementToXMLSubscription_id $ subscription_subscription_id x
            , elementToXMLStatus $ subscription_status x
            , maybe [] (elementToXMLCancellation_reason) $ subscription_cancellation_reason x
            , elementToXMLUnderlying_sku_id $ subscription_underlying_sku_id x
            , elementToXMLShopper_id $ subscription_shopper_id x
            , maybe [] (elementToXMLRecurring_charge) $ subscription_recurring_charge x
            , maybe [] (elementToXMLCredit_card) $ subscription_credit_card x
            , maybe [] (elementToXMLEcp) $ subscription_ecp x
            , maybe [] (elementToXMLPaypal) $ subscription_paypal x
            , maybe [] (elementToXMLPaypal_subscription) $ subscription_paypal_subscription x
            , maybe [] (elementToXMLCatalog_recurring_charge) $ subscription_catalog_recurring_charge x
            , maybe [] (elementToXMLOverride_recurring_charge) $ subscription_override_recurring_charge x
            , maybe [] (elementToXMLCoupon) $ subscription_coupon x
            , maybe [] (elementToXMLCharge_frequency) $ subscription_charge_frequency x
            , elementToXMLNext_charge_date $ subscription_next_charge_date x
            , maybe [] (elementToXMLManual_recurring_info) $ subscription_manual_recurring_info x
            , maybe [] (elementToXMLAuto_renew) $ subscription_auto_renew x
            , maybe [] (elementToXMLLast_charge_result) $ subscription_last_charge_result x
            , maybe [] (elementToXMLSubscription_charges) $ subscription_subscription_charges x
            ]
 
elementSubscription :: XMLParser Subscription
elementSubscription = parseSchemaType "subscription"
elementToXMLSubscription :: Subscription -> [Content ()]
elementToXMLSubscription = schemaTypeToXML "subscription"
 
data Subscription_id = Subscription_id
        deriving (Eq,Show)
instance SchemaType Subscription_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Subscription_id
    schemaTypeToXML s x@Subscription_id{} =
        toXMLElement s []
            []
 
elementSubscription_id :: XMLParser Xs.Long
elementSubscription_id = parseSchemaType "subscription-id"
elementToXMLSubscription_id :: Xs.Long -> [Content ()]
elementToXMLSubscription_id = schemaTypeToXML "subscription-id"
 
data Underlying_sku_id = Underlying_sku_id
        deriving (Eq,Show)
instance SchemaType Underlying_sku_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Underlying_sku_id
    schemaTypeToXML s x@Underlying_sku_id{} =
        toXMLElement s []
            []
 
elementUnderlying_sku_id :: XMLParser Xs.Long
elementUnderlying_sku_id = parseSchemaType "underlying-sku-id"
elementToXMLUnderlying_sku_id :: Xs.Long -> [Content ()]
elementToXMLUnderlying_sku_id = schemaTypeToXML "underlying-sku-id"
 
data Recurring_charge = Recurring_charge
        { recurring_charge_currency :: Xsd.XsdString
        , recurring_charge_amount :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Recurring_charge where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Recurring_charge
            `apply` elementCurrency
            `apply` elementAmount
    schemaTypeToXML s x@Recurring_charge{} =
        toXMLElement s []
            [ elementToXMLCurrency $ recurring_charge_currency x
            , elementToXMLAmount $ recurring_charge_amount x
            ]
 
elementRecurring_charge :: XMLParser Recurring_charge
elementRecurring_charge = parseSchemaType "recurring-charge"
elementToXMLRecurring_charge :: Recurring_charge -> [Content ()]
elementToXMLRecurring_charge = schemaTypeToXML "recurring-charge"
 
data Next_charge_date = Next_charge_date
        deriving (Eq,Show)
instance SchemaType Next_charge_date where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Next_charge_date
    schemaTypeToXML s x@Next_charge_date{} =
        toXMLElement s []
            []
 
elementNext_charge_date :: XMLParser Xsd.XsdString
elementNext_charge_date = parseSchemaType "next-charge-date"
elementToXMLNext_charge_date :: Xsd.XsdString -> [Content ()]
elementToXMLNext_charge_date = schemaTypeToXML "next-charge-date"
 
data Catalog_recurring_charge = Catalog_recurring_charge
        { catalog_recurring_charge_currency :: Xsd.XsdString
        , catalog_recurring_charge_amount :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Catalog_recurring_charge where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Catalog_recurring_charge
            `apply` elementCurrency
            `apply` elementAmount
    schemaTypeToXML s x@Catalog_recurring_charge{} =
        toXMLElement s []
            [ elementToXMLCurrency $ catalog_recurring_charge_currency x
            , elementToXMLAmount $ catalog_recurring_charge_amount x
            ]
 
elementCatalog_recurring_charge :: XMLParser Catalog_recurring_charge
elementCatalog_recurring_charge = parseSchemaType "catalog-recurring-charge"
elementToXMLCatalog_recurring_charge :: Catalog_recurring_charge -> [Content ()]
elementToXMLCatalog_recurring_charge = schemaTypeToXML "catalog-recurring-charge"
 
data Override_recurring_charge = Override_recurring_charge
        { override_recurring_charge_currency :: Xsd.XsdString
        , override_recurring_charge_amount :: Xsd.Decimal
        }
        deriving (Eq,Show)
instance SchemaType Override_recurring_charge where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Override_recurring_charge
            `apply` elementCurrency
            `apply` elementAmount
    schemaTypeToXML s x@Override_recurring_charge{} =
        toXMLElement s []
            [ elementToXMLCurrency $ override_recurring_charge_currency x
            , elementToXMLAmount $ override_recurring_charge_amount x
            ]
 
elementOverride_recurring_charge :: XMLParser Override_recurring_charge
elementOverride_recurring_charge = parseSchemaType "override-recurring-charge"
elementToXMLOverride_recurring_charge :: Override_recurring_charge -> [Content ()]
elementToXMLOverride_recurring_charge = schemaTypeToXML "override-recurring-charge"
 
data Charge_frequency = Charge_frequency
        deriving (Eq,Show)
instance SchemaType Charge_frequency where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Charge_frequency
    schemaTypeToXML s x@Charge_frequency{} =
        toXMLElement s []
            []
 
elementCharge_frequency :: XMLParser Xsd.XsdString
elementCharge_frequency = parseSchemaType "charge-frequency"
elementToXMLCharge_frequency :: Xsd.XsdString -> [Content ()]
elementToXMLCharge_frequency = schemaTypeToXML "charge-frequency"
 
elementAuto_renew :: XMLParser Xsd.Boolean
elementAuto_renew = parseSchemaType "auto-renew"
elementToXMLAuto_renew :: Xsd.Boolean -> [Content ()]
elementToXMLAuto_renew = schemaTypeToXML "auto-renew"
 
data Last_charge_result = Last_charge_result
        { last_charge_result_result_code :: Xsd.XsdString
        , last_charge_result_processor_error_message :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Last_charge_result where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Last_charge_result
            `apply` elementResult_code
            `apply` optional (elementProcessor_error_message)
    schemaTypeToXML s x@Last_charge_result{} =
        toXMLElement s []
            [ elementToXMLResult_code $ last_charge_result_result_code x
            , maybe [] (elementToXMLProcessor_error_message) $ last_charge_result_processor_error_message x
            ]
 
elementLast_charge_result :: XMLParser Last_charge_result
elementLast_charge_result = parseSchemaType "last-charge-result"
elementToXMLLast_charge_result :: Last_charge_result -> [Content ()]
elementToXMLLast_charge_result = schemaTypeToXML "last-charge-result"
 
data Manual_recurring_info = Manual_recurring_info
        { manual_recurring_info_original_reference_number :: Maybe Xs.Long
        , manual_recurring_info_reminder_number :: Maybe Xsd.XsdString
        , manual_recurring_info_payment_method :: Maybe Xsd.XsdString
        , manual_recurring_info_next_payment_url :: Maybe Xsd.XsdString
        , manual_recurring_info_days_till_cancel :: Maybe Xs.Long
        , manual_recurring_info_days_till_payment :: Maybe Xs.Long
        , manual_recurring_info_email_sent :: Maybe Xsd.Boolean
        }
        deriving (Eq,Show)
instance SchemaType Manual_recurring_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Manual_recurring_info
            `apply` optional (elementOriginal_reference_number)
            `apply` optional (elementReminder_number)
            `apply` optional (elementPayment_method)
            `apply` optional (elementNext_payment_url)
            `apply` optional (elementDays_till_cancel)
            `apply` optional (elementDays_till_payment)
            `apply` optional (elementEmail_sent)
    schemaTypeToXML s x@Manual_recurring_info{} =
        toXMLElement s []
            [ maybe [] (elementToXMLOriginal_reference_number) $ manual_recurring_info_original_reference_number x
            , maybe [] (elementToXMLReminder_number) $ manual_recurring_info_reminder_number x
            , maybe [] (elementToXMLPayment_method) $ manual_recurring_info_payment_method x
            , maybe [] (elementToXMLNext_payment_url) $ manual_recurring_info_next_payment_url x
            , maybe [] (elementToXMLDays_till_cancel) $ manual_recurring_info_days_till_cancel x
            , maybe [] (elementToXMLDays_till_payment) $ manual_recurring_info_days_till_payment x
            , maybe [] (elementToXMLEmail_sent) $ manual_recurring_info_email_sent x
            ]
 
elementManual_recurring_info :: XMLParser Manual_recurring_info
elementManual_recurring_info = parseSchemaType "manual-recurring-info"
elementToXMLManual_recurring_info :: Manual_recurring_info -> [Content ()]
elementToXMLManual_recurring_info = schemaTypeToXML "manual-recurring-info"
 
data Days_till_cancel = Days_till_cancel
        deriving (Eq,Show)
instance SchemaType Days_till_cancel where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Days_till_cancel
    schemaTypeToXML s x@Days_till_cancel{} =
        toXMLElement s []
            []
 
elementDays_till_cancel :: XMLParser Xs.Long
elementDays_till_cancel = parseSchemaType "days-till-cancel"
elementToXMLDays_till_cancel :: Xs.Long -> [Content ()]
elementToXMLDays_till_cancel = schemaTypeToXML "days-till-cancel"
 
data Days_till_payment = Days_till_payment
        deriving (Eq,Show)
instance SchemaType Days_till_payment where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Days_till_payment
    schemaTypeToXML s x@Days_till_payment{} =
        toXMLElement s []
            []
 
elementDays_till_payment :: XMLParser Xs.Long
elementDays_till_payment = parseSchemaType "days-till-payment"
elementToXMLDays_till_payment :: Xs.Long -> [Content ()]
elementToXMLDays_till_payment = schemaTypeToXML "days-till-payment"
 
data Original_reference_number = Original_reference_number
        deriving (Eq,Show)
instance SchemaType Original_reference_number where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Original_reference_number
    schemaTypeToXML s x@Original_reference_number{} =
        toXMLElement s []
            []
 
elementOriginal_reference_number :: XMLParser Xs.Long
elementOriginal_reference_number = parseSchemaType "original-reference-number"
elementToXMLOriginal_reference_number :: Xs.Long -> [Content ()]
elementToXMLOriginal_reference_number = schemaTypeToXML "original-reference-number"
 
data Next_payment_url = Next_payment_url
        deriving (Eq,Show)
instance SchemaType Next_payment_url where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Next_payment_url
    schemaTypeToXML s x@Next_payment_url{} =
        toXMLElement s []
            []
 
elementNext_payment_url :: XMLParser Xsd.XsdString
elementNext_payment_url = parseSchemaType "next-payment-url"
elementToXMLNext_payment_url :: Xsd.XsdString -> [Content ()]
elementToXMLNext_payment_url = schemaTypeToXML "next-payment-url"
 
data Reminder_number = Reminder_number
        deriving (Eq,Show)
instance SchemaType Reminder_number where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Reminder_number
    schemaTypeToXML s x@Reminder_number{} =
        toXMLElement s []
            []
 
elementReminder_number :: XMLParser Xsd.XsdString
elementReminder_number = parseSchemaType "reminder-number"
elementToXMLReminder_number :: Xsd.XsdString -> [Content ()]
elementToXMLReminder_number = schemaTypeToXML "reminder-number"
 
data Email_sent = Email_sent
        deriving (Eq,Show)
instance SchemaType Email_sent where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Email_sent
    schemaTypeToXML s x@Email_sent{} =
        toXMLElement s []
            []
 
elementEmail_sent :: XMLParser Xsd.Boolean
elementEmail_sent = parseSchemaType "email-sent"
elementToXMLEmail_sent :: Xsd.Boolean -> [Content ()]
elementToXMLEmail_sent = schemaTypeToXML "email-sent"
 
data Subscription_charges = Subscription_charges
        { subscription_charges_url :: [Xsd.XsdString]
        , subscription_charges_subscription_charge :: [Subscription_charge]
        }
        deriving (Eq,Show)
instance SchemaType Subscription_charges where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Subscription_charges
            `apply` many (elementUrl)
            `apply` many (elementSubscription_charge)
    schemaTypeToXML s x@Subscription_charges{} =
        toXMLElement s []
            [ concatMap (elementToXMLUrl) $ subscription_charges_url x
            , concatMap (elementToXMLSubscription_charge) $ subscription_charges_subscription_charge x
            ]
 
elementSubscription_charges :: XMLParser Subscription_charges
elementSubscription_charges = parseSchemaType "subscription-charges"
elementToXMLSubscription_charges :: Subscription_charges -> [Content ()]
elementToXMLSubscription_charges = schemaTypeToXML "subscription-charges"
 
data Result_code = Result_code
        deriving (Eq,Show)
instance SchemaType Result_code where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Result_code
    schemaTypeToXML s x@Result_code{} =
        toXMLElement s []
            []
 
elementResult_code :: XMLParser Xsd.XsdString
elementResult_code = parseSchemaType "result-code"
elementToXMLResult_code :: Xsd.XsdString -> [Content ()]
elementToXMLResult_code = schemaTypeToXML "result-code"
 
data Processor_error_message = Processor_error_message
        deriving (Eq,Show)
instance SchemaType Processor_error_message where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Processor_error_message
    schemaTypeToXML s x@Processor_error_message{} =
        toXMLElement s []
            []
 
elementProcessor_error_message :: XMLParser Xsd.XsdString
elementProcessor_error_message = parseSchemaType "processor-error-message"
elementToXMLProcessor_error_message :: Xsd.XsdString -> [Content ()]
elementToXMLProcessor_error_message = schemaTypeToXML "processor-error-message"
 
data Cancellation_reason = Cancellation_reason
        deriving (Eq,Show)
instance SchemaType Cancellation_reason where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Cancellation_reason
    schemaTypeToXML s x@Cancellation_reason{} =
        toXMLElement s []
            []
 
elementCancellation_reason :: XMLParser Xsd.XsdString
elementCancellation_reason = parseSchemaType "cancellation-reason"
elementToXMLCancellation_reason :: Xsd.XsdString -> [Content ()]
elementToXMLCancellation_reason = schemaTypeToXML "cancellation-reason"
 
data Three_d_authenticated_info = Three_d_authenticated_info
        { three_d_authenticated_info_transaction_id :: Xsd.XsdString
        , three_d_authenticated_info_payer_authentication_response :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Three_d_authenticated_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Three_d_authenticated_info
            `apply` elementTransaction_id
            `apply` elementPayer_authentication_response
    schemaTypeToXML s x@Three_d_authenticated_info{} =
        toXMLElement s []
            [ elementToXMLTransaction_id $ three_d_authenticated_info_transaction_id x
            , elementToXMLPayer_authentication_response $ three_d_authenticated_info_payer_authentication_response x
            ]
 
elementThree_d_authenticated_info :: XMLParser Three_d_authenticated_info
elementThree_d_authenticated_info = parseSchemaType "three-d-authenticated-info"
elementToXMLThree_d_authenticated_info :: Three_d_authenticated_info -> [Content ()]
elementToXMLThree_d_authenticated_info = schemaTypeToXML "three-d-authenticated-info"
 
data Transaction_id = Transaction_id
        deriving (Eq,Show)
instance SchemaType Transaction_id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Transaction_id
    schemaTypeToXML s x@Transaction_id{} =
        toXMLElement s []
            []
 
elementTransaction_id :: XMLParser Xsd.XsdString
elementTransaction_id = parseSchemaType "transaction-id"
elementToXMLTransaction_id :: Xsd.XsdString -> [Content ()]
elementToXMLTransaction_id = schemaTypeToXML "transaction-id"
 
data Payer_authentication_response = Payer_authentication_response
        deriving (Eq,Show)
instance SchemaType Payer_authentication_response where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Payer_authentication_response
    schemaTypeToXML s x@Payer_authentication_response{} =
        toXMLElement s []
            []
 
elementPayer_authentication_response :: XMLParser Xsd.XsdString
elementPayer_authentication_response = parseSchemaType "payer-authentication-response"
elementToXMLPayer_authentication_response :: Xsd.XsdString -> [Content ()]
elementToXMLPayer_authentication_response = schemaTypeToXML "payer-authentication-response"
 
data Web_info = Web_info
        { web_info_ip :: [Xsd.XsdString]
        , web_info_remote_host :: [Xsd.XsdString]
        , web_info_user_agent :: [Xsd.XsdString]
        , web_info_accept_language :: [Xsd.XsdString]
        }
        deriving (Eq,Show)
instance SchemaType Web_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Web_info
            `apply` between (Occurs Nothing (Just 1))
                            (elementIp)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementRemote_host)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementUser_agent)
            `apply` between (Occurs (Just 0) (Just 1))
                            (elementAccept_language)
    schemaTypeToXML s x@Web_info{} =
        toXMLElement s []
            [ concatMap (elementToXMLIp) $ web_info_ip x
            , concatMap (elementToXMLRemote_host) $ web_info_remote_host x
            , concatMap (elementToXMLUser_agent) $ web_info_user_agent x
            , concatMap (elementToXMLAccept_language) $ web_info_accept_language x
            ]
 
elementWeb_info :: XMLParser Web_info
elementWeb_info = parseSchemaType "web-info"
elementToXMLWeb_info :: Web_info -> [Content ()]
elementToXMLWeb_info = schemaTypeToXML "web-info"
 
data Ip = Ip
        deriving (Eq,Show)
instance SchemaType Ip where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Ip
    schemaTypeToXML s x@Ip{} =
        toXMLElement s []
            []
 
elementIp :: XMLParser Xsd.XsdString
elementIp = parseSchemaType "ip"
elementToXMLIp :: Xsd.XsdString -> [Content ()]
elementToXMLIp = schemaTypeToXML "ip"
 
data Remote_host = Remote_host
        deriving (Eq,Show)
instance SchemaType Remote_host where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Remote_host
    schemaTypeToXML s x@Remote_host{} =
        toXMLElement s []
            []
 
elementRemote_host :: XMLParser Xsd.XsdString
elementRemote_host = parseSchemaType "remote-host"
elementToXMLRemote_host :: Xsd.XsdString -> [Content ()]
elementToXMLRemote_host = schemaTypeToXML "remote-host"
 
data User_agent = User_agent
        deriving (Eq,Show)
instance SchemaType User_agent where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return User_agent
    schemaTypeToXML s x@User_agent{} =
        toXMLElement s []
            []
 
elementUser_agent :: XMLParser Xsd.XsdString
elementUser_agent = parseSchemaType "user-agent"
elementToXMLUser_agent :: Xsd.XsdString -> [Content ()]
elementToXMLUser_agent = schemaTypeToXML "user-agent"
 
data Accept_language = Accept_language
        deriving (Eq,Show)
instance SchemaType Accept_language where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Accept_language
    schemaTypeToXML s x@Accept_language{} =
        toXMLElement s []
            []
 
elementAccept_language :: XMLParser Xsd.XsdString
elementAccept_language = parseSchemaType "accept-language"
elementToXMLAccept_language :: Xsd.XsdString -> [Content ()]
elementToXMLAccept_language = schemaTypeToXML "accept-language"
 
data Cart_info = Cart_info
        { cart_info_cart_params :: [Cart_params]
        , cart_info_cart :: [Cart]
        }
        deriving (Eq,Show)
instance SchemaType Cart_info where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Cart_info
            `apply` between (Occurs Nothing (Just 1))
                            (elementCart_params)
            `apply` between (Occurs Nothing (Just 1))
                            (elementCart)
    schemaTypeToXML s x@Cart_info{} =
        toXMLElement s []
            [ concatMap (elementToXMLCart_params) $ cart_info_cart_params x
            , concatMap (elementToXMLCart) $ cart_info_cart x
            ]
 
elementCart_info :: XMLParser Cart_info
elementCart_info = parseSchemaType "cart-info"
elementToXMLCart_info :: Cart_info -> [Content ()]
elementToXMLCart_info = schemaTypeToXML "cart-info"
 
data Cart_params = Cart_params
        { cart_params_currency :: Xsd.XsdString
        , cart_params_ip :: Xsd.XsdString
        , cart_params_country :: Xsd.XsdString
        , cart_params_state :: Xsd.XsdString
        , cart_params_store_id :: Xs.Long
        }
        deriving (Eq,Show)
instance SchemaType Cart_params where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Cart_params
            `apply` elementCurrency
            `apply` elementIp
            `apply` elementCountry
            `apply` elementState
            `apply` elementStore_id
    schemaTypeToXML s x@Cart_params{} =
        toXMLElement s []
            [ elementToXMLCurrency $ cart_params_currency x
            , elementToXMLIp $ cart_params_ip x
            , elementToXMLCountry $ cart_params_country x
            , elementToXMLState $ cart_params_state x
            , elementToXMLStore_id $ cart_params_store_id x
            ]
 
elementCart_params :: XMLParser Cart_params
elementCart_params = parseSchemaType "cart-params"
elementToXMLCart_params :: Cart_params -> [Content ()]
elementToXMLCart_params = schemaTypeToXML "cart-params"
