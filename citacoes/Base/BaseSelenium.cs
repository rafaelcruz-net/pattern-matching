using System.Collections.Generic;
using OpenQA.Selenium;
using OpenQA.Selenium.Chrome;
using OpenQA.Selenium.Firefox;

namespace citacoes.Base
{
    public class BaseSelenium 
    {
        public IWebDriver Driver { get; set; }

        public BaseSelenium()
        {
            var firefoxOptions = new FirefoxOptions();
            this.Driver = new FirefoxDriver(firefoxOptions);
        }

        public void Navegate(string url = "")
        {
            this.Driver.Url = url;
            this.Driver.Navigate();
        }

        public void Close() 
        {
            this.Driver.Quit();
        }

        public IList<IWebElement> GetElementsByCssClass(string @class) =>
            this.Driver.FindElements(By.ClassName(@class));

        public IList<IWebElement> GetElementsByCssSelector(string cssSelector) =>
            this.Driver.FindElements(By.CssSelector(cssSelector));

        public IWebElement GetElementByCssClass(string @class) =>
            this.Driver.FindElement(By.ClassName(@class));

        public IWebElement GetElementById(string id) =>
            this.Driver.FindElement(By.Id(id));

    }
}