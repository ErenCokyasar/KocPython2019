import random

class Portfolio():
    def __init__(self):
        self.cash = 0
        self.StockList = []
        self.MutualFundList = []
        self.TransactionList = []

    def addCash(self, x):
        self.cash += x

        transaction = Transaction(1, x, "", 0)
        self.TransactionList.append(transaction)

    def withdrawCash(self, x):
        self.cash -= x

        transaction = Transaction(2, x, "", 0)
        self.TransactionList.append(transaction)

    def buyStock(self, a, s):
        flag = 0
        for i in self.StockList:
            if i.ticker == s.ticker:
                flag = 1
                i.amount += a
        if flag == 0:
            s.amount = a
            self.StockList.append(s)
        self.cash -= a * s.price_buy

        transaction = Transaction(3, a, s.ticker, s.price_buy)
        self.TransactionList.append(transaction)

    def buyMutualFund(self, a, m):
        flag = 0
        for i in self.MutualFundList:
            if i.ticker == m.ticker:
                flag = 1
                i.amount += a
        if flag == 0:
            m.amount = a
            self.MutualFundList.append(m)
        self.cash -= a

        transaction = Transaction(4, a, m.ticker, 1)
        self.TransactionList.append(transaction)

    def sellStock(self, str, a):
        for s in self.StockList:
            if s.ticker == str:
                s.amount -= a
                self.cash += a * s.price_sold
                transaction = Transaction(5, a, str, s.price_sold)
                self.TransactionList.append(transaction)

    def sellMutualFund(self, str, a):
        for m in self.MutualFundList:
            if m.ticker == str:
                m.amount -= a
                self.cash += a * m.price_sold
                transaction = Transaction(6, a, str, m.price_sold)
                self.TransactionList.append(transaction)

    def current(self):
        print("Current cash is:")
        print(self.cash)

        print("Current stocks are:")
        for s in self.StockList:
            str = repr(s.amount) + ' shares of ' + s.ticker + ' stock with $' + repr(s.price_sold) + '/share selling price'
            print(str)

        print("Current mutual funds are:")
        for m in self.MutualFundList:
            str = repr(m.amount) + ' shares of ' + m.ticker + ' mutual fund with $' + repr(m.price_sold) + '/share selling price'
            print(str)

    def history(self):
        print("TRANSACTIONS HISTORY:")
        for t in self.TransactionList:
            if t.type == 1:
                str = 'Cash added - $' + repr(t.x1)
                print(str)
            elif t.type == 2:
                str = 'Cash withdrawed - $' + repr(t.x1)
                print(str)
            elif t.type == 3:
                str = 'Bought ' + repr(t.x1) + ' shares of ' + repr(t.x2) + ' stock with $' + repr(t.x3) + '/share buying price'
                print(str)
            elif t.type == 4:
                str = 'Bought ' + repr(t.x1) + ' shares of ' + repr(t.x2) + ' mutual fund with $' + repr(t.x3) + '/share buying price'
                print(str)
            elif t.type == 5:
                str = 'Sold ' + repr(t.x1) + ' shares of ' + repr(t.x2) + ' stock with $' + repr(t.x3) + '/share selling price'
                print(str)
            else:
                str = 'Sold ' + repr(t.x1) + ' shares of ' + repr(t.x2) + ' mutual fund with $' + repr(t.x3) + '/share selling price'
                print(str)

class Stock():
    def __init__(self, p, str):
        self.price_buy = p
        self.ticker = str
        self.amount = 0
        self.price_sold = random.uniform(p/2,3*p/2)

class MutualFund():
    def __init__(self, str):
        self.ticker = str
        self.amount = 0
        self.price_sold = random.uniform(0.9,1.2)

class Transaction():
    def __init__(self, t, a, b, c):
        self.type = t
        self.x1 = a
        self.x2 = b
        self.x3 = c

portfolio = Portfolio()
portfolio.addCash(300.50)
s = Stock(20, "HFH")
portfolio.buyStock(5, s)
mf1 = MutualFund("BRT")
mf2 = MutualFund("GHT")
portfolio.buyMutualFund(10.3, mf1)
portfolio.buyMutualFund(2, mf2)
portfolio.sellMutualFund("BRT", 3)
portfolio.sellStock("HFH", 1)
portfolio.withdrawCash(50)
portfolio.current()
portfolio.history()


