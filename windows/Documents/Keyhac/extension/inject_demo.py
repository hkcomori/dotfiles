#!/usr/bin/env python3

from vendor.injector import (
    inject,
    Injector,
)


class Keymap:
    def hoge(self):
        return self.__class__.__name__


class WindowFactory:
    def hoge(self):
        return self.__class__.__name__


class WindowFactoryKeyhac(WindowFactory):
    def hoge(self):
        return self.fuga()

    def fuga(self):
        return 'fuga'


class ActionService:
    @inject
    def __init__(self, keymap: Keymap, window_factory: WindowFactory) -> None:
        self._keymap = keymap
        self._window_factory = window_factory

    def perform(self):
        print(self._keymap.hoge(), self._window_factory.hoge())


class KeymapRegistry:
    def hoge(self):
        raise NotImplementedError


class KeymapRegistryKeyhac(KeymapRegistry):
    @inject
    def __init__(self, keymap: Keymap, query: str) -> None:
        self._keymap = keymap
        self._query = query

    def hoge(self):
        print(self._keymap.hoge())
        print(self._query)


class Controller:
    @inject
    def __init__(self, registry: KeymapRegistry, action_service: ActionService) -> None:
        self._action_service = action_service
        self._registry = registry

    def run(self):
        self._action_service.perform()
        self._registry.hoge()


class Dependency():
    def __init__(self, keymap: Keymap) -> None:
        self._keymap = keymap
        self.injector = Injector(self.config)

    def config(self, binder):
        binder.bind(WindowFactory, WindowFactoryKeyhac)
        binder.bind(KeymapRegistry, KeymapRegistryKeyhac)
        binder.bind(Keymap, self._keymap)

    def resolve(self, cls, query):
        self.injector.binder.bind(str, query)
        return self.injector.get(cls)


if __name__ == '__main__':
    keymap = Keymap()
    injector = Dependency(keymap)
    controller1 = injector.resolve(Controller, 'hello world')
    controller2 = injector.resolve(Controller, 'good morning')
    controller1.run()
    controller2.run()
